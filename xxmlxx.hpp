#pragma once
#include <tuple>
#include <format>
#include <string>
#include <ranges>
#include <vector>
#include <utility>
#include <variant>
#include <optional>
#include <algorithm>
#include <string_view>

namespace xxmlxx {

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    ///                                                               XML Core Part                                                                    ///
    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    namespace internal_fmt_flags {
        static constexpr std::uint8_t none         = 0;
        static constexpr std::uint8_t self_tag     = 1;
        static constexpr std::uint8_t open_tag     = 2;
    }
    
    typedef struct element_attribute {
        std::string key;
        std::string value;

        inline std::string to_string() const {
            return std::format("{:s}=\"{:s}\"", key, value);
        } 
        
    } element_attribute;

    typedef struct node_data_comment {
        std::string name;

        constexpr node_data_comment() = default;
        constexpr node_data_comment(std::string_view n) : name(n.data()) {}

        template <std::uint8_t internal_flag>
        constexpr inline std::string to_string() const {
            if constexpr (internal_flag & internal_fmt_flags::open_tag) {
                return std::format("<!--{:s}-->", name);
            }
            return {};
        }
    } node_data_comment;

    typedef struct node_data_element {
        std::string                    name;
        std::string                    content;
        std::vector<element_attribute> attributes;

        constexpr inline node_data_element() = default;
        constexpr inline node_data_element(std::string_view n, std::string_view c = "", const std::vector<element_attribute>& a = {}) : name(n.data()), content(c.data()), attributes(a) {}

        constexpr inline std::string attributes_to_string() const {
            std::string buffer;
            for (const auto& a : attributes) {
                buffer.push_back(' ');
                buffer.append(a.to_string());
            }
            return buffer;
        }
        
        template <std::uint8_t internal_flag>
        constexpr inline std::string to_string() const {
            if constexpr (internal_flag & internal_fmt_flags::self_tag) {
                if (content.empty()) { return std::format("<{:s}{:s}/>", name, attributes_to_string()); }
                return std::format("<{0:s}{1:s}>{2:s}</{0:s}>", name, attributes_to_string(), content);
            }
            if constexpr (internal_flag & internal_fmt_flags::open_tag) {
                return std::format("<{:s}{:s}>{:s}", name, attributes_to_string(), content);
            } else {
                return std::format("</{:s}>", name);
            }
        }
        
    } node_data_element;

    static constexpr struct node_data_flag_comment { } node_comment;
    static constexpr struct node_data_flag_element { } node_element;

    typedef std::variant<
        node_data_comment,
        node_data_element> node_data;
    
    typedef std::vector<element_attribute> attributes_t;
    
    class tree_node {
        std::ptrdiff_t     parent_id_ = 0;
        node_data          data_;
    public:
        using iterator       = tree_node*;
        using const_iterator = const tree_node*;
        
        tree_node() = default;
        tree_node(const tree_node&)           = default;
        tree_node(tree_node&&)                 = default;
        tree_node& operator=(const tree_node&) = default;
        tree_node& operator=(tree_node&&)      = default;
        ~tree_node() = default;

        template <class Flag, typename ... Args>
        constexpr inline tree_node(Flag f, std::ptrdiff_t pid, Args&& ... args) : parent_id_(pid), data_() {
            if constexpr (std::is_convertible_v<decltype(node_comment), Flag>) {
                data_.emplace<node_data_comment>(std::forward<Args>(args)...);
            }
            if constexpr (std::is_convertible_v<decltype(node_element), Flag>) {
                data_.emplace<node_data_element>(std::forward<Args>(args)...);
            }
        }
        
        constexpr std::ptrdiff_t parent_id() const { return parent_id_; }

        constexpr inline std::size_t depth(const_iterator begin) const {
            std::ptrdiff_t d = 0;
            for (auto it = this; it->parent_id_ != -1; it = begin + it->parent_id_) { ++d; }
            return static_cast<std::size_t>(d);
        }

        template <std::uint8_t internal_flag>
        constexpr inline std::string to_string(std::size_t d) const {
            std::string space(d << 1, ' ');
            return std::visit([&space](auto&& f) { return std::format("{:s}{:s}\n", space, f.template to_string<internal_flag>()); }, data_);
        }

        constexpr inline std::string_view name() const {
            return std::visit([](auto&& f) { return std::string_view(f.name); }, data_);
        }

        constexpr inline tree_node*        name(std::string_view n) {
            std::visit([n](auto&& f) { return f.name = n; }, data_);
            return this;
        }

        constexpr inline tree_node*        text(std::string_view t) {
            std::get<node_data_element>(data_).content = t;
            return this;
        }

        constexpr inline std::string_view text() const {
            return std::string_view(std::get<node_data_element>(data_).content);
        }

        constexpr inline tree_node*       push_attribute(std::string_view key, std::string_view val) {
            std::get<node_data_element>(data_).attributes.emplace_back(key.data(), val.data());
            return this;
        }

        constexpr inline tree_node*       attribute(const std::vector<element_attribute>& attr) {
            std::get<node_data_element>(data_).attributes = attr;
            return this;
        }

        constexpr inline auto             begin_attribute() {
            return std::get<node_data_element>(data_).attributes.begin();
        }
        
        constexpr inline auto             end_attribute() {
            return std::get<node_data_element>(data_).attributes.end();
        }

        constexpr inline auto             find_attribute(std::string_view key) {
            return std::ranges::find_if(std::get<node_data_element>(data_).attributes, [key](const auto& a) {
                return key == a.key;
            });
        }
    };

    class document_tree {
        std::vector<tree_node> nodes_;
    public:
        using iterator       = tree_node::iterator;
        using const_iterator = tree_node::const_iterator;
        using node_type      = tree_node;
        
        document_tree(std::string_view root_name, std::size_t cap = 1 << 10) {
            nodes_.reserve(cap);
            nodes_.emplace_back(node_element, -1, root_name);
        }
        document_tree(const document_tree&)            = default;
        document_tree(document_tree&&)                 = default;
        document_tree& operator=(const document_tree&) = default;
        document_tree& operator=(document_tree&&)      = default;
        ~document_tree()                      = default;

        constexpr inline iterator begin() { return &nodes_.front(); }
        constexpr inline iterator end()   { return (&nodes_.back() + 1); }
        constexpr inline iterator last()   { return (&nodes_.back()); }

        constexpr inline const_iterator begin() const { return &nodes_.front(); }
        constexpr inline const_iterator end()   const { return (&nodes_.back() + 1); }
        constexpr inline const_iterator last()  const { return (&nodes_.back()); }

        template <class Flag, typename ... Args>
        constexpr inline iterator push_node(const Flag f, iterator parent, Args&& ... args) {
            return &nodes_.emplace_back(f, parent - begin(), std::forward<Args>(args)...);
        }

        template <class Flag, typename ... Args>
        constexpr inline iterator push_node(const Flag f, Args&& ... args) {
            return push_node<Flag>(f, begin(), std::forward<Args>(args)...);
        }

        // Since child must be pushed to tree after this parent.
        // We will only search those nodes after it.
        
        constexpr inline iterator find_first_child(iterator b) {
            return std::find_if(b + 1, end(), [b, this](const auto& it) {
                return it.parent_id() == b - begin();
            });
        }

        constexpr inline iterator find_first_child_with_name(iterator b, std::string_view name) {
            return std::find_if(b + 1, end(), [b, this, name](const auto& it) {
                return it.parent_id() == (b - begin()) && it.name() == name;
            });
        }

        constexpr inline iterator find_first_child_with_attribute(iterator b, std::string_view key, std::string_view value) {
            return std::find_if(b + 1, end(), [b, this, key, value](auto it) {
                auto a = it.find_attribute(key);
                return it.parent_id() == (b - begin()) && a != it.end_attribute() && a->key == key && a->value == value;
            });
        }
        
        constexpr inline iterator find_first_sibling(iterator b) {
            return std::find_if(b + 1, end(), [b](const auto& it) {
                return it.parent_id() == b->parent_id();
            });
        }

        constexpr inline iterator find_first_sibling_with_name(iterator b) {
            return std::find_if(b + 1, end(), [b](const auto& it) {
                return it.parent_id() == b->parent_id() && it.name() == b->name();
            });
        }

        constexpr inline iterator find_first_sibling_with_attribute(iterator b, std::string_view key, std::string_view value) {
            return std::find_if(b + 1, end(), [b, key, value](auto& it) {
                auto a = it.find_attribute(key);
                return b->parent_id() == it.parent_id() && a != it.end_attribute() && a->key == key && a->value == value;
            });
        }

        constexpr inline iterator find_nth_sibling(iterator b, std::size_t n) {
            for (std::size_t i = 0; b != end() && i != n; b = find_first_sibling(b)) { ++i; }
            return b;
        }

        constexpr inline iterator find_nth_sibling_with_name(iterator b, std::size_t n) {
            for (std::size_t i = 0; b != end() && i != n; b = find_first_sibling_with_name(b)) { ++i; }
            return b;
        }

        constexpr inline std::size_t node_depth(const tree_node& n) const {
            return n.depth(begin());
        }

        constexpr inline std::size_t depth() const {
            return std::ranges::max_element(*this, [this](const auto& a, const auto& b) {
                return a.depth(begin()) < b.depth(begin());
            })->depth(begin());
        }

        constexpr inline std::size_t node_count() const {
            return nodes_.size();
        }

        ///////////////////////////////////////////////////////////////////////
        ///                       XML Output Part                           ///
        ///                   Only one function, yes...                     ///
        ///////////////////////////////////////////////////////////////////////

        inline std::string to_string() const {
            static constexpr std::string_view format_string = R"(<?xml version="1.0" encoding="utf-8"?>)""\n{:s}";
            // A single root node doesn't require iterations.
            if (depth() == 0) {
                return std::format(format_string, begin()->to_string<internal_fmt_flags::self_tag>(0));
            }

            std::vector<std::string> node_string_dense(nodes_.size());
            for (std::size_t i = depth(); i != 0; i--) {
                auto                     same_depth_view = *this | std::views::filter([i, this](auto& e) { return node_depth(e)  == i; });
                auto                     parent_ids_view = same_depth_view | std::views::transform([](auto& e) { return e.parent_id(); });
                for (auto parent_id : parent_ids_view) {
                    if (node_string_dense[parent_id].empty()) {
                        std::string element_cache;
                        element_cache.append(nodes_[parent_id].to_string<internal_fmt_flags::open_tag>(i - 1));
                        for (auto& j : same_depth_view | std::views::filter([parent_id](auto& e) { return e.parent_id() == parent_id; })) {
                            if (node_string_dense[&j - begin()].empty()) {
                                element_cache.append(j.to_string<internal_fmt_flags::self_tag | internal_fmt_flags::open_tag>(i));
                            } else {
                                element_cache += node_string_dense[&j - begin()];
                            }
                        }
                        element_cache.append(nodes_[parent_id].to_string<internal_fmt_flags::none>(i - 1));
                        node_string_dense[parent_id] = element_cache;
                    }
                }
            }
            return std::format(format_string, node_string_dense.front());
        }
    };

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    ///                                                               XML Parsing Part                                                                 ///
    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    enum class segment_type : std::uint8_t {
        characters, comment, open, close, self
    };

    static constexpr std::string_view segment_to_string(segment_type e) {
        constexpr std::string_view names[] = {
            "characters", "comment", "open", "close", "self"
        };
        return names[static_cast<std::uint8_t>(e)];
    }

    struct     segment_info_type {
        segment_type                   type;
        std::string_view               name;
        std::vector<element_attribute> attributes;
    };

    ///////////////////////////////////////////////////////////////////////////////
    ///                     Parser Combinator Namespace                         ///
    ///////////////////////////////////////////////////////////////////////////////
    
    namespace pc {
        
        // When C++23 comes this would be replaced by std::expected.
        template <class Ty>
        class expected_handle {
            Ty          value_;
            std::string error_msg_;
        public:
            using value_type = Ty;
            
            constexpr expected_handle(const Ty& value)       : value_(value), error_msg_() {}
            constexpr expected_handle(Ty&& value)            : value_(std::move(value)), error_msg_() {}
            constexpr expected_handle()                      : value_(), error_msg_() {}
            constexpr expected_handle(std::string_view msg)  : error_msg_(msg) {}
            
            constexpr expected_handle(const expected_handle&)             = default;
            constexpr expected_handle(expected_handle&&)                  = default;
            constexpr expected_handle& operator=(const expected_handle&)  = default;
            constexpr expected_handle& operator=(expected_handle&&)       = default;

            constexpr value_type&  operator*()       {  return value_; }
            constexpr value_type   operator*() const {  return value_; }

            constexpr value_type*        operator->()       {  return &value_; }
            constexpr const value_type*  operator->() const {  return &value_; }

            constexpr std::string_view   error() const                 {  return error_msg_; }
            constexpr void               error(std::string_view e)     { error_msg_ = e; }

            constexpr operator bool() const noexcept { return error_msg_.empty(); }
        };
        
        template <class Ty>
        using result       = expected_handle<std::pair<Ty, std::string_view>>;

        template <class Parser>
        concept parser = requires(Parser ps, std::string_view input) {
            { ps(input) } -> std::same_as<result<typename Parser::result_type>>;
        };

        namespace details {
            
            // Some meta functions.
            template <class Ty>
            struct as_tuple_type { using type = std::tuple<Ty>; };

            template <class ... Args>
            struct as_tuple_type<std::tuple<Args...>> { using type = std::tuple<Args...>; };

            template <class T1, class T2>
            struct tuple_cat_type                                       { using type = std::tuple<T1, T2>; };

            template <class T1, class ... T2>
            struct tuple_cat_type<T1, std::tuple<T2...>>                { using type = std::tuple<T1, T2...>; };

            template <class ... T1, class T2>
            struct tuple_cat_type<std::tuple<T1...>, T2>                { using type = decltype(std::tuple_cat(std::tuple<T1...>{}, std::tuple<T2>{})); };

            template <class ... T1, class ... T2>
            struct tuple_cat_type<std::tuple<T1...>, std::tuple<T2...>> { using type = decltype(std::tuple_cat(std::tuple<T1...>{}, std::tuple<T2...>{})); };

            template <class Ty>
            struct remove_variant_type { using type = Ty; };

            template <class Ty>
            struct remove_variant_type<std::variant<Ty>> { using type = Ty; };

            template <class Ty>
            struct as_variant_type { using type = std::variant<Ty>; };

            template <class ... Types>
            struct as_variant_type<std::variant<Types ...>> { using type = std::variant<Types ...>; };

            template <class T1, class T2>
            struct variant_cat_type                                       { using type = std::variant<T1, T2>; };

            template <class T1, class ... T2>
            struct variant_cat_type<T1, std::variant<T2...>>                { using type = std::variant<T1, T2...>; };

            template <class ... T1, class T2>
            struct variant_cat_type<std::variant<T1...>, T2>                { using type = std::variant<T1..., T2>; };
            
            template <class ... T1, class ... T2>
            struct variant_cat_type<std::variant<T1...>, std::variant<T2...>>                { using type = std::variant<T1 ..., T2 ...>; };

            template <typename Variant, typename = void>
            struct unique_variant;

            template <typename... Ts>
            struct unique_variant<std::variant<Ts...>, void> {
                template <typename T, typename... Us>
                static constexpr bool contains = (std::is_same_v<T, Us> || ...);

                template <typename... Acc>
                struct impl {
                    using type = std::variant<Acc...>;
                };

                template <typename... Acc, typename U, typename... Rest>
                struct impl<std::variant<Acc...>, U, Rest...> {
                    using type = std::conditional_t<
                        contains<U, Acc...>,
                        typename impl<std::variant<Acc...>, Rest...>::type,
                        typename impl<std::variant<Acc..., U>, Rest...>::type
                    >;
                };

                // Since recursive will add one more variant, so will remove it manually.
                using type = typename remove_variant_type<typename impl<std::variant<>, Ts...>::type>::type ;
            };
            
            // Some basic parser implementation.
            template <bool Inversed>
            struct parser_string_range_character_impl {
                using                 result_type = std::string_view;
                std::string_view      range;
                
                constexpr     result<result_type> operator()(std::string_view input) const {
                    if constexpr (!Inversed) {
                        if (input.empty() || range.find(input.front()) == std::string_view::npos) { return std::string_view("One of string matched failed!"); }
                    } else {
                        if (input.empty() || range.find(input.front()) != std::string_view::npos) { return std::string_view("One of string inversed matched failed!"); }
                    }
                    return std::make_pair(input.substr(0, 1), input.substr(1));
                }
            };

            template <bool Inversed>
            struct parser_string_range_string_impl {
                using                 result_type = std::string_view;
                std::string_view      range;

                constexpr     result<result_type> operator()(std::string_view input) const {
                    std::size_t off = 0;
                    if constexpr (!Inversed) {
                        off = input.find_first_not_of(range);
                    } else {
                        off = input.find_first_of(range);
                    }
                    off = off == std::string_view::npos ? input.size() : off;
                    return std::make_pair(input.substr(0, off), input.substr(off));
                }
            };

            struct parser_string_impl {
                using              result_type = std::string_view;
                std::string_view   pattern;
                constexpr          result<result_type> operator()(std::string_view input) const {
                    if (input.starts_with(pattern)) { return std::make_pair(input.substr(0, pattern.size()), input.substr(pattern.size())); }
                    return std::string_view("Sequence string matched failed!");
                }
            };

            // Parser operator impl
            template <parser P1, parser P2>
            struct parser_continuation_impl {
                using          result_type = typename tuple_cat_type<typename P1::result_type, typename P2::result_type>::type;
                P1             parser_first;
                P2             parser_second;
                constexpr      result<result_type> operator()(std::string_view input) const {
                    if (auto r1 = parser_first(input); r1) {
                        auto [v1, remain1] = *r1;
                        if (auto r2 = parser_second(remain1); r2) {
                            auto [v2, remain2] = *r2;
                            return std::make_pair(
                                std::tuple_cat(
                                    typename as_tuple_type<typename P1::result_type>::type{v1},
                                    typename as_tuple_type<typename P2::result_type>::type{v2}),
                                remain2);
                        } else {
                            return r2.error();
                        }
                    } else {
                        return r1.error();
                    }
                }
            };

            template <parser P1, parser P2>
            struct parser_variation_impl {
                using          result_type = typename unique_variant<typename variant_cat_type<typename P1::result_type, typename P2::result_type>::type>::type;
                P1             parser_first;
                P2             parser_second;

                constexpr      result<result_type> operator()(std::string_view input) const {
                    if (auto res1 = parser_first(input); res1) {
                        return std::make_pair(std::visit([](auto&& v) -> result_type { return v; }, typename as_variant_type<typename P1::result_type>::type{res1->first}), res1->second);
                    }
                    if (auto res2 = parser_second(input); res2) {
                        return std::make_pair(std::visit([](auto&& v) -> result_type { return v; }, typename as_variant_type<typename P2::result_type>::type{res2->first}), res2->second);
                    }
                    return std::string_view("operator| matched failed!");
                }
            };

            template <parser Parser>
            struct parser_optional_impl {
                using          result_type = std::optional<typename Parser::result_type>;
                Parser         parser;
                constexpr      result<result_type> operator()(std::string_view input) const {
                    if (auto r = parser(input); r) { return std::make_pair(std::make_optional(r->first), r->second); }
                    return std::pair<std::optional<typename Parser::result_type>, std::string_view>(std::nullopt, input);
                }
            };

            template <parser Parser>
            struct parser_many_impl {
                using          result_type = std::vector<typename Parser::result_type>;
                Parser         parser;
                constexpr      result<result_type> operator()(std::string_view input) const {
                    result_type result;
                    for (auto res = parser(input); res; res = parser(input)) {
                        auto [val, remain] = *res;
                        result.emplace_back(val);
                        input = remain;
                    }
                    return std::make_pair(result, input);
                }
            };

            template <parser Parser>
            struct parser_many_counted_impl {
                using          result_type = std::vector<typename Parser::result_type>;
                Parser         parser;
                std::size_t    limit;
                constexpr      result<result_type> operator()(std::string_view input) const {
                    result_type result(limit);
                    for (std::size_t i = 0; i != limit; ++i) {
                        if (auto res = parser(input); res) {
                            auto [val, remain] = *res;
                            result[i] = val;
                            input = remain;
                        } else {
                            return std::string_view("operator* with to less pattern showed!");
                        }
                    }
                    return std::make_pair(result, input);
                }
            };

            template <parser Parser>
            struct parser_group_impl {
                using          result_type = std::tuple<typename Parser::result_type>;
                Parser         parser;
                constexpr      result<result_type> operator()(std::string_view input) const {
                    if (auto r = parser(input); r) {
                        return std::make_pair(std::make_tuple(r->first), r->second);
                    } else {
                        return r.error();
                    }
                }
            };
        }

        namespace literals {
            
            template <parser P1, parser P2>
            static constexpr auto operator,(const P1& p1, const P2& p2) { return details::parser_continuation_impl{p1, p2}; }

            template <parser P1, parser P2>
            static constexpr auto operator|(const P1& p1, const P2& p2) { return details::parser_variation_impl{p1, p2}; }

            template <parser Parser>
            static constexpr auto operator~(const Parser& p) { return details::parser_optional_impl{p}; }

            template <parser Parser>
            static constexpr auto operator*(const Parser& p) { return details::parser_many_impl{p}; }

            template <parser Parser>
            static constexpr auto operator*(std::size_t n, const Parser& p) { return details::parser_many_counted_impl{p, n}; }

            template <parser Parser>
            static constexpr auto operator+(const Parser& p) {  return details::parser_group_impl{p}; }

            template <bool Inversed>
            static constexpr auto operator!(const details::parser_string_range_character_impl<Inversed>& p) {
                return details::parser_string_range_character_impl<!Inversed>{p.range};
            }

            template <bool Inversed>
            static constexpr auto operator!(const details::parser_string_range_string_impl<Inversed>& p) {
                return details::parser_string_range_string_impl<!Inversed>{p.range};
            }

            // lot_of string operator.
            static constexpr auto operator""_ls(const char* input, std::size_t size) {
                return details::parser_string_range_string_impl<false>{std::string_view(input, size)};
            }

            // one_of string operator.
            static constexpr auto operator""_os(const char* input, std::size_t size) {
                return details::parser_string_range_character_impl<false>{std::string_view(input, size)}; 
            }

            // sequence is string operator.
            static constexpr auto operator""_ss(const char* input, std::size_t size) {
                return details::parser_string_impl{std::string_view(input, size)};
            }
            
        }
        
        using namespace pc::literals;

        static constexpr auto ws = " \n\t\r"_ls;
        static constexpr auto name = +("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"_os, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_."_ls);
        static constexpr auto attribute_value      = +(("\""_ss, !"\""_ls, "\""_ss) | ("'"_ss, !"'"_ls, "'"_ss));
        static constexpr auto attribute = (" \n\t\r"_os, ws, name, ws, "="_ss, ws, attribute_value);
        static constexpr auto comment_tag = ("<!--"_ss, ~"-"_ss, !"-"_ls, "-->"_ss);
        static constexpr auto open_tag = ("<"_ss, name, *(attribute), ws, ">"_ss);
        static constexpr auto close_tag = ("</"_ss, name, ">"_ss);
        static constexpr auto self_tag = ("<"_ss, name, *(attribute), ws, "/"_ss, ws, ">"_ss); // To make is return type different from open_tag.
        static constexpr auto declaration_tag = (ws, R"(<?xml version="1.0" encoding="utf-8"?>)"_ss, ws);

        static constexpr auto segment = comment_tag | open_tag | close_tag | self_tag | !"<&"_ls; // last one is char data.

        static constexpr segment_info_type segment_info(const decltype(comment_tag)::result_type& comment) {
            // We would ignore comments.
            return {
                segment_type::comment, "", {}
            };
        }

        static constexpr segment_info_type segment_info(const decltype(open_tag)::result_type& open) {
            auto transformed_view = std::get<2>(open) | std::views::transform([](const auto& v) {
                return element_attribute{ std::string(&std::get<1>(std::get<2>(v)).front() - 1, std::get<1>(std::get<2>(v)).size() + 1),
                    std::visit([](auto&& v) { return std::string(std::get<1>(v)); }, std::get<6>(v)) };
            });
            return  {
                segment_type::open,
                std::string_view(&std::get<0>(std::get<1>(open)).front(), std::get<1>(std::get<1>(open)).size() + 1),
                std::vector<element_attribute>(transformed_view.begin(), transformed_view.end())
            };
        }

        static constexpr segment_info_type segment_info(const decltype(close_tag)::result_type& close) {
            return {
                segment_type::close,
                std::string_view(&std::get<0>(std::get<1>(close)).front(), std::get<1>(std::get<1>(close)).size() + 1), {}
            };
        }

        static constexpr segment_info_type segment_info(const decltype(self_tag)::result_type& self) {
            auto transformed_view = std::get<2>(self) | std::views::transform([](const auto& v) {
                return element_attribute{ std::string(&std::get<1>(std::get<2>(v)).front() - 1, std::get<1>(std::get<2>(v)).size() + 1),
                    std::visit([](auto&& v) { return std::string(std::get<1>(v)); }, std::get<6>(v))  };
            });
            return {
                segment_type::self,
                std::string_view(&std::get<0>(std::get<1>(self)).front(), std::get<1>(std::get<1>(self)).size() + 1),
                std::vector<element_attribute>(transformed_view.begin(), transformed_view.end())
            };
        }

        static constexpr segment_info_type segment_info(const std::string_view&  char_data) {
            return {
                segment_type::characters, char_data, {}
            };
        }
    }

    // Predefine this so we can use it as return value inside document_parser.
    class parser_segment_iterator;

    class document_parser {
        friend class parser_segment_iterator;
        using segment_type = decltype(pc::segment)::result_type;
        
        std::string_view    doc_;
        bool                last_add_one_;
        bool                time_to_add_one_;
        constexpr std::string_view skip_decl() {
            auto r = pc::declaration_tag(doc_);
            if (r) { doc_ = r->second; }
            return r.error();
        }

        constexpr std::string_view next_segment(segment_type& bk) {
            // First jump through all spaces.
            if (!time_to_add_one_) {
                doc_ = pc::ws(doc_)->second;
                auto r = pc::segment(doc_);
                if (r) { bk = r->first; doc_ = r->second; }
                return r.error();
            }
            return "";
        }

        constexpr bool not_end() {
            if (!doc_.empty() || !last_add_one_ || time_to_add_one_) {
                return !doc_.empty();
            }
            time_to_add_one_ = true;
            return true;
        }
        
    public:
        constexpr explicit document_parser(std::string_view document) noexcept
        : doc_(document), last_add_one_(document.back() == '>'), time_to_add_one_(false) {}

        constexpr std::string_view        document() const noexcept { return doc_; }
        constexpr parser_segment_iterator to_tree(document_tree&);

    };

    class parser_segment_iterator {
        document_parser*                  parser_ = nullptr;
        bool                              not_end_ = false;
        std::string_view                  error_;
        document_parser::segment_type     segment_;
    public:
        constexpr explicit parser_segment_iterator(document_parser& parser) noexcept : parser_(&parser), not_end_(false), error_(), segment_() {
            if (auto r = parser.skip_decl(); r.empty()) {
                if (auto s = parser.next_segment(segment_); s.empty()) {
                    not_end_ = true;
                } else { error_ = s; }
            } else { error_ = r; }
        }
        constexpr explicit parser_segment_iterator() = default;
        constexpr explicit parser_segment_iterator(std::string_view e) noexcept : error_(e) {}

        constexpr parser_segment_iterator(const parser_segment_iterator&) noexcept = default;
        constexpr parser_segment_iterator(parser_segment_iterator&&)      noexcept = default;
        constexpr parser_segment_iterator& operator=(const parser_segment_iterator&) noexcept = default;
        constexpr parser_segment_iterator& operator=(parser_segment_iterator&&) noexcept = default;

        constexpr bool operator==(const parser_segment_iterator& other) const { return not_end_ == other.not_end_; }
        constexpr bool operator!=(const parser_segment_iterator& other) const { return not_end_ != other.not_end_; }

        constexpr auto operator*()  const noexcept { return segment_; }
        constexpr auto operator->() const noexcept { return &segment_; }

        constexpr parser_segment_iterator& operator++() {
            error_   = parser_->next_segment(segment_);
            not_end_ = parser_->not_end();
            return *this;
        }
        constexpr parser_segment_iterator  operator++(int) { auto tmp = *this; ++*this; return tmp; }

        constexpr segment_info_type  info() const {
            return std::visit([](auto&& v) {
                return pc::segment_info(v);
            }, segment_);
        }

        constexpr operator bool() const noexcept { return error_.empty(); }
        constexpr std::string_view error() const noexcept { return error_; }
    };

    //////////////////////////////////////////////////////////////////////////
    ///                         XML Input Part                             ///
    ///                 Still only one function, yes...                    ///
    //////////////////////////////////////////////////////////////////////////
    
    constexpr parser_segment_iterator document_parser::to_tree(document_tree& tree_mem) {
        // Cache the index inside tree, an elegant way to avoid recursive. "inspired by Dijkstra's Double Stack Algorithm"
        std::vector<std::size_t>   segment_stack; 
        segment_stack.reserve(1024);
        parser_segment_iterator    current_segment(*this);
        
        if (current_segment.info().type != xxmlxx::segment_type::open && current_segment.info().type != xxmlxx::segment_type::self) {
            return parser_segment_iterator("Root node (first node) can not be comment or anything else!");
        }
        // Add root first because our tree doesn't support pushing root after construction.
        tree_mem.begin()->name(current_segment.info().name)->attribute(current_segment.info().attributes);
        segment_stack.push_back(0);
        ++current_segment;
        
        for (;current_segment != parser_segment_iterator(); ++current_segment) {
            if (!current_segment) {
                return parser_segment_iterator(current_segment.error());
            }
            auto info = current_segment.info();
            switch (info.type) {
            case xxmlxx::segment_type::characters:
                tree_mem.last()->text(info.name);
                break;
            case xxmlxx::segment_type::comment:
                break;
            case xxmlxx::segment_type::open:
                tree_mem.push_node(node_element, tree_mem.begin() + segment_stack.back(), info.name, "", info.attributes);
                segment_stack.push_back(tree_mem.node_count() - 1);
                break;
            case xxmlxx::segment_type::close:
                if ((tree_mem.begin() + segment_stack.back())->name() != info.name) {
                    return parser_segment_iterator("Unmatched element!"); 
                }
                segment_stack.pop_back();
                break;
            case xxmlxx::segment_type::self:
                tree_mem.push_node(node_element, tree_mem.begin() + segment_stack.back(), info.name, "", info.attributes);
                break;
            default:
                return parser_segment_iterator("Not a valid segment type");
            }
        }
        return current_segment;
    }
}