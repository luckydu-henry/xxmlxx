#pragma once

#include <vector>
#include <ranges>
#include <format>
#include <variant>
#include <cstring>
#include <optional>
#include <algorithm>

namespace xxmlxx {
    
    namespace details {

        // For output and input tag verify.
        enum class document_tag_category : std::uint16_t {
            unknow   = 0,
            open     = 1,
            self     = 2,
            close    = 3,
            text     = 4,
            comment  = 5
        };

        static constexpr std::string_view document_tag_category_string(document_tag_category category) {
            constexpr std::string_view a[] = {"unknow", "open", "self", "close", "text", "comment"};
            return a[static_cast<std::underlying_type_t<document_tag_category>>(category)];
        }

        ///////////////////////////////////////////////////////////////////////////////
        ///                       Parser Combinator Part                            ///
        ///////////////////////////////////////////////////////////////////////////////

        // When C++23 comes this would be replaced by std::expected.
        template <class Ty>
        class expected_handle {
            Ty                  value_;
            std::string_view    error_msg_;
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
        using parser_result       = expected_handle<std::pair<Ty, std::string_view>>;

        template <class Parser>
        concept parser = requires(Parser ps, std::string_view input, void* any_context) {
            { ps(input, any_context) } -> std::same_as<parser_result<typename Parser::result_type>>;
        };

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

            template <class Context>
            constexpr     parser_result<result_type> operator()(std::string_view input, Context& ctx) const {
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

            template <class Context>
            constexpr     parser_result<result_type> operator()(std::string_view input, Context& ctx) const {
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

            template <class Context>
            constexpr          parser_result<result_type> operator()(std::string_view input, Context& ctx) const {
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

            template <class Context>
            constexpr      parser_result<result_type> operator()(std::string_view input, Context& ctx) const {
                if (auto r1 = parser_first(input, ctx); r1) {
                    auto [v1, remain1] = *r1;
                    if (auto r2 = parser_second(remain1, ctx); r2) {
                        auto [v2, remain2] = *r2;
                        return std::make_pair(
                            std::tuple_cat(
                                typename as_tuple_type<typename P1::result_type>::type{v1},
                                typename as_tuple_type<typename P2::result_type>::type{v2}),
                            remain2);
                    } else { return r2.error(); }
                } else { return r1.error(); }
            }
        };

        template <parser P1, parser P2>
        struct parser_variation_impl {
            using          result_type = typename unique_variant<typename variant_cat_type<typename P1::result_type, typename P2::result_type>::type>::type;
            P1             parser_first;
            P2             parser_second;

            template <class Context>
            constexpr      parser_result<result_type> operator()(std::string_view input, Context& ctx) const {
                if (auto res1 = parser_first(input, ctx); res1) {
                    return std::make_pair(std::visit([](auto&& v) -> result_type { return v; }, typename as_variant_type<typename P1::result_type>::type{res1->first}), res1->second);
                }
                if (auto res2 = parser_second(input, ctx); res2) {
                    return std::make_pair(std::visit([](auto&& v) -> result_type { return v; }, typename as_variant_type<typename P2::result_type>::type{res2->first}), res2->second);
                }
                return std::string_view("operator| matched failed!");
            }
        };

        template <parser Parser>
        struct parser_optional_impl {
            using          result_type = std::optional<typename Parser::result_type>;
            Parser         parser;

            template <class Context>
            constexpr      parser_result<result_type> operator()(std::string_view input, Context& ctx) const {
                if (auto r = parser(input, ctx); r) { return std::make_pair(std::make_optional(r->first), r->second); }
                return std::pair<std::optional<typename Parser::result_type>, std::string_view>(std::nullopt, input);
            }
        };

        template <parser Parser, class Action>
        struct parser_action_impl {
            using result_type = typename Parser::result_type;
            Parser       parser;
            Action       action;

            template <class Context>
            constexpr    parser_result<result_type> operator()(std::string_view input, Context& ctx) const {
                if (auto res = parser(input, ctx); res) {
                    action(input, res, ctx);
                    return res;
                } else { return res.error(); }
            }
        };
            
        template <parser P1, parser P2>
        static constexpr auto operator,(const P1& p1, const P2& p2) { return details::parser_continuation_impl<P1, P2>{p1, p2}; }

        template <parser P1, parser P2>
        static constexpr auto operator|(const P1& p1, const P2& p2) { return details::parser_variation_impl<P1,P2>{p1, p2}; }

        template <parser Parser>
        static constexpr auto operator~(const Parser& p) { return details::parser_optional_impl<Parser>{p}; }
        
        template <parser Parser, class Action>
        static constexpr auto operator%(const Parser& p, Action act) { return details::parser_action_impl<Parser, Action>{p, act}; }

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

        // White space parser.
        static constexpr auto parse_ws = " \n\t\r"_ls;
        static constexpr auto parse_name = ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"_os, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_."_ls);
        static constexpr auto parse_attribute = (" \n\t\r"_os, parse_ws, parse_name, parse_ws, "="_ss, parse_ws, (("\""_ss, !"\""_ls, "\""_ss) | ("'"_ss, !"'"_ls, "'"_ss)));

        ////////////////////////////////////////////////////////////////
        //                Define parser combinators                   //
        ////////////////////////////////////////////////////////////////

        // Align makes faster
        
        struct segment_context {
            document_tag_category       category;
            std::string                 buffer;

            constexpr segment_context() : category(document_tag_category::unknow), buffer() {
                buffer.reserve(2048);
            }
        };

        struct parser_attribute_pairs {
            using result_type = int;

            template <class Context>
            constexpr      parser_result<result_type> operator()(std::string_view input, Context& ctx) const {
                
                for (auto res = parse_attribute(input, ctx);
                    res;
                    res = parse_attribute(input, ctx)) {
                    auto [val, remain] = *res;
                    auto name_front = std::get<2>(val);
                    auto name_rest  = std::get<3>(val);
                    auto aval = std::visit([](auto&& a) { return std::get<1>(a); }, std::get<7>(val));

                    ctx.buffer.push_back(' ');
                    ctx.buffer.push_back(name_front.front());
                    ctx.buffer.append(name_rest);
                    ctx.buffer.append("=\"");
                    ctx.buffer.append(aval);
                    ctx.buffer.push_back('\"');
                    
                    input = remain;
                }
                return std::make_pair(int(), input);
            }
        };

#define CONTEXT(ctx) [](auto&& input, auto&& result, segment_context& ctx)
        static constexpr auto parse_tag_decl = (parse_ws, R"(<?xml version="1.0" encoding="utf-8"?>)"_ss, parse_ws);
        static constexpr auto parse_tag_comment = ("<!--"_ss, ~"-"_ss, !"-"_ls, "-->"_ss) % CONTEXT(ctx) {
            ctx.category = document_tag_category::comment;
        };

        static constexpr auto parse_tag_open_or_self = ("<"_ss, parse_name, parser_attribute_pairs{}, parse_ws, ~"/"_ss, ">"_ss) % CONTEXT(ctx) {
            ctx.category     = std::get<5>(result->first) ? document_tag_category::self : document_tag_category::open;
            auto name_front  = std::get<1>(result->first);
            auto name_rest   = std::get<2>(result->first);
            // Because we will call parser first then action.
            ctx.buffer.insert(0, name_rest);
            ctx.buffer.insert(ctx.buffer.begin(), name_front.front());
        };

        static constexpr auto parse_tag_close = ("</"_ss, parse_name, ">"_ss) % CONTEXT(ctx) {
            ctx.category     = document_tag_category::close;
            auto name_front  = std::get<1>(result->first);
            auto name_rest   = std::get<2>(result->first);

            ctx.buffer.push_back(name_front.front());
            ctx.buffer.append(name_rest);
        };

        static constexpr auto parse_tag_text = !"<&"_ls % CONTEXT(ctx) {
            ctx.category = document_tag_category::text;
            ctx.buffer.append(result->first);
        };
#undef CONTEXT
        // The actual parser we would call.
        static constexpr auto parse_segment = parse_tag_comment | parse_tag_open_or_self | parse_tag_close  | parse_tag_text;

        // Pass this to the second argument of parser if has no context.
        static constexpr int  no_context = 0;
    }

    enum class tree_node_category : std::uint8_t {
        unknow  = 0,
        element = 1,
        text    = 2,
        comment = 3
    };
    
    template <class StrAllocator>
    class tree_node {
    public:
        using allocator_type      = StrAllocator;
        using string_type         = std::basic_string<char, std::char_traits<char>, StrAllocator>;
        using pointer             = tree_node*;
        using const_pointer       = const tree_node*;
        using reference           = tree_node&;
        using const_reference     = const tree_node&;
    private:
        std::int32_t                                                          parent_index_ = 0;
        tree_node_category                                                    category_     = tree_node_category::unknow;
        std::uint8_t                                                          depth_        = 0;
        std::basic_string<char, std::char_traits<char>, StrAllocator>         buffer_;

        constexpr std::size_t          find_attribute_(std::string_view key) const {
            std::size_t id = buffer_.find(key);
            for (; id != std::string_view::npos; id = buffer_.find(key, id + key.size())) {
                if (buffer_[id + key.size()] == '=' && buffer_[id + key.size() + 1] == '\"') {
                    return id + key.size() + 2;
                }
            }
            return std::string_view::npos;
        }

        constexpr void                 replace_attribute_(std::size_t off, std::string_view new_val) {
            if (std::size_t end = buffer_.find('\"', off); end != std::string_view::npos) {
                buffer_.replace(off, end - off, new_val);
            }
        }

        constexpr void                 push_attribute_(std::string_view k, std::string_view v) {
            buffer_.push_back(' ');
            buffer_.append(k);
            buffer_.append("=\"");
            buffer_.append(v);
            buffer_.push_back('\"');
        }

        constexpr std::string_view     get_attribute_(std::size_t off) const {
            return std::string_view(buffer_.data() + off, buffer_.find('\"', off) - off);
        }
        
    public:
        
        constexpr tree_node(pointer begin, tree_node_category cat, std::int32_t pid, std::string_view node_content, const StrAllocator& allocator)
            : parent_index_(pid), category_(cat), depth_(0), buffer_(allocator){
            buffer_.reserve(256);
            buffer_.assign(node_content);
            for (auto it = this; it->parent_index_ != -1; it = begin + it->parent_index_) { ++depth_; }
        }

        constexpr tree_node() = default;
        constexpr tree_node(const tree_node&) = default;
        constexpr tree_node(tree_node&&) = default;
        constexpr tree_node& operator=(const tree_node&) = default;
        constexpr tree_node& operator=(tree_node&&) = default;
        constexpr ~tree_node() = default;

        constexpr std::int32_t       parent_id() const { return parent_index_; }
        constexpr std::int32_t&      parent_id()       { return parent_index_; }
        
        constexpr tree_node_category category()  const { return category_; }

        constexpr pointer           buffer(std::string_view s) {
            buffer_.assign(s);
            return this;
        }

        constexpr std::string_view  buffer() const {
            return buffer_;
        }

        constexpr auto               get_allocator() const { return buffer_.get_allocator(); }
        
        constexpr std::size_t        depth() const {
            return static_cast<std::size_t>(depth_);
        }

        constexpr std::string_view   name() const {
            if (category_ == tree_node_category::element) [[likely]] {
                const std::size_t       first_space  = buffer_.find(' ');
                const std::size_t       length       = first_space == std::string_view::npos ? buffer_.size() : first_space;
                return std::string_view(buffer_.data(), length);
            }
            return {};
        }

        constexpr pointer           attribute(std::string_view k, std::string_view v) {
            if (category_ == tree_node_category::element) [[likely]] {
                if (std::size_t id = find_attribute_(k); id != std::string_view::npos) {
                    replace_attribute_(id, v);
                } else {
                    push_attribute_(k, v);
                }
            }
            return this;
        }

        constexpr std::string_view   attribute(std::string_view k) const {
            if (category_ == tree_node_category::element) [[likely]] {
                if (auto id = find_attribute_(k); id != std::string_view::npos) {
                    return get_attribute_(id);
                }
            }
            return {};
        }

        constexpr pointer           text(std::string_view s) {
            if (category_ == tree_node_category::text) [[likely]] {
                return buffer(s);
            }
            return this;
        }

        constexpr pointer           comment(std::string_view s) {
            if (category_ == tree_node_category::comment) [[likely]] {
                return buffer(s);
            }
            return this;
        }

        constexpr std::string_view   text() const {
            return category_ == tree_node_category::text ? std::string_view(buffer_.data(), buffer_.size()) : std::string_view();
        }

        constexpr std::string_view   comment() const {
            return category_ == tree_node_category::comment ? std::string_view(buffer_.data(), buffer_.size()) : std::string_view();
        }

        template <details::document_tag_category TagCat, class OutputIt>
        constexpr auto               format_to(OutputIt it) const -> OutputIt {
            switch (category_) {
            case tree_node_category::unknow: break;
            case tree_node_category::element:
                if constexpr (TagCat == details::document_tag_category::open) {
                    return std::format_to(it, "<{:s}>\n", buffer_);
                }
                if constexpr (TagCat == details::document_tag_category::close) {
                    return std::format_to(it, "</{:s}>\n", name());
                }
                if constexpr (TagCat == details::document_tag_category::self) {
                    return std::format_to(it, "<{:s}/>\n",buffer_);
                } break;
            case tree_node_category::text:
                if constexpr (TagCat == details::document_tag_category::open || TagCat == details::document_tag_category::self) {
                    return std::format_to(it, "{:s}",          buffer_);
                } break;
            case tree_node_category::comment:
                if constexpr (TagCat == details::document_tag_category::open || TagCat == details::document_tag_category::self) {
                    return std::format_to(it, "<!--{:s}-->\n",   buffer_);
                } break;
            }
            return std::format_to(it, "\n");
        }
    };


    template <class DocTree>
    class document_tree_const_iterator {
    public:
        using tree_type         = DocTree;
        using difference_type   = std::ptrdiff_t;
        using iterator_category = std::random_access_iterator_tag;
        using iterator_concept  = std::contiguous_iterator_tag;
        
        using value_type        = typename tree_type::value_type;
        using pointer           = typename tree_type::const_pointer;
        using reference         = typename tree_type::const_reference;
    protected:
        pointer       node_ptr_ = nullptr;
        tree_type*    tree_ptr_ = nullptr;
    public:
        ~document_tree_const_iterator() = default;
        constexpr document_tree_const_iterator(const document_tree_const_iterator&) = default;
        constexpr document_tree_const_iterator(document_tree_const_iterator&&) = default;
        constexpr document_tree_const_iterator() = default;
        constexpr document_tree_const_iterator(const tree_type* const tp, const pointer np)
            : node_ptr_(const_cast<pointer>(np)), tree_ptr_(const_cast<tree_type*>(tp)) {}

        constexpr document_tree_const_iterator& operator=(const document_tree_const_iterator&) = default;
        constexpr document_tree_const_iterator& operator=(document_tree_const_iterator&&)      = default;

        constexpr reference       operator*()               const { return *node_ptr_; }
        constexpr pointer         operator->()              const { return node_ptr_; }
        constexpr reference       operator[](std::size_t i) const { return node_ptr_[i]; }

        constexpr document_tree_const_iterator& operator++()                             { ++node_ptr_; return *this; }
        constexpr document_tree_const_iterator  operator++(int)                          { return document_tree_const_iterator{node_ptr_++, tree_ptr_}; }
        constexpr document_tree_const_iterator& operator+=(const std::ptrdiff_t d)       { node_ptr_ += d; return *this; }
        constexpr document_tree_const_iterator  operator+ (const std::ptrdiff_t d) const { return document_tree_const_iterator{node_ptr_ + d, tree_ptr_}; }

        constexpr document_tree_const_iterator& operator--()                             { --node_ptr_; return *this; }
        constexpr document_tree_const_iterator  operator--(int)                          { return document_tree_const_iterator{node_ptr_--, tree_ptr_}; }
        constexpr document_tree_const_iterator& operator-=(const std::ptrdiff_t d)       { node_ptr_ -= d; return *this; }
        constexpr document_tree_const_iterator  operator- (const std::ptrdiff_t d) const { return document_tree_const_iterator{node_ptr_ - d, tree_ptr_}; }
        
        constexpr bool operator==(const document_tree_const_iterator& right) const { return tree_ptr_ == right.tree_ptr_ && node_ptr_ == right.node_ptr_; }
        constexpr bool operator!=(const document_tree_const_iterator& right) const { return node_ptr_ != right.node_ptr_ || tree_ptr_ != right.tree_ptr_; }
        constexpr bool operator< (const document_tree_const_iterator& right) const { return tree_ptr_ == right.tree_ptr_ && node_ptr_ <  right.node_ptr_; }
        constexpr bool operator> (const document_tree_const_iterator& right) const { return tree_ptr_ == right.tree_ptr_ && node_ptr_ >  right.node_ptr_; }
        constexpr bool operator<=(const document_tree_const_iterator& right) const { return tree_ptr_ == right.tree_ptr_ && node_ptr_ <= right.node_ptr_; }
        constexpr bool operator>=(const document_tree_const_iterator& right) const { return tree_ptr_ == right.tree_ptr_ && node_ptr_ >= right.node_ptr_; }

        constexpr std::ptrdiff_t                      operator-(const document_tree_const_iterator& rhs) const { return node_ptr_ - rhs.node_ptr_; }
        constexpr friend document_tree_const_iterator operator+(const std::ptrdiff_t d, const document_tree_const_iterator& it) { return it + d; }
        
    };
    
    template <class DocTree>
    class document_tree_iterator : protected document_tree_const_iterator<DocTree> {
    public:
        using base = document_tree_const_iterator<DocTree>;
        using tree_type         = typename base::tree_type;
        using difference_type   = typename base::difference_type;
        using iterator_category = typename base::iterator_category;
        using iterator_concept  = typename base::iterator_concept;
        
        using value_type        = typename tree_type::value_type;
        using pointer           = typename tree_type::pointer;
        using reference         = typename tree_type::reference;
        
        ~document_tree_iterator() = default;
        constexpr document_tree_iterator(const document_tree_iterator&) = default;
        constexpr document_tree_iterator(document_tree_iterator&&) = default;
        constexpr document_tree_iterator() = default;
        constexpr document_tree_iterator(const tree_type* const tp, const pointer np) : base(tp, np) {}

        constexpr document_tree_iterator& operator=(const document_tree_iterator&) = default;
        constexpr document_tree_iterator& operator=(document_tree_iterator&&) = default;

        constexpr reference       operator*()               const { return const_cast<reference>(base::operator*()); }
        constexpr pointer         operator->()              const { return const_cast<pointer>  (base::operator->()); }
        constexpr reference       operator[](std::size_t i) const { return const_cast<reference>(base::operator[](i)); }

        constexpr document_tree_iterator& operator++()                             { base::operator++(); return *this; }
        constexpr document_tree_iterator  operator++(int)                          { return base::operator++(0); }
        constexpr document_tree_iterator& operator+=(const std::ptrdiff_t d)       { base::operator+=(d); return *this; }
        constexpr document_tree_iterator  operator+ (const std::ptrdiff_t d) const { return base::operator+(d); }

        constexpr document_tree_iterator& operator--()                             { base::operator--(); return *this;  }
        constexpr document_tree_iterator  operator--(int)                          { return base::operator--(0); }
        constexpr document_tree_iterator& operator-=(const std::ptrdiff_t d)       { base::operator-=(d); return *this; }
        constexpr document_tree_iterator  operator- (const std::ptrdiff_t d) const { return base::operator-(d); }

        constexpr bool operator==(const document_tree_iterator& right) const { return base::operator==(right); }
        constexpr bool operator!=(const document_tree_iterator& right) const { return base::operator!=(right); }
        constexpr bool operator< (const document_tree_iterator& right) const { return base::operator<(right); }
        constexpr bool operator> (const document_tree_iterator& right) const { return base::operator>(right); }
        constexpr bool operator<=(const document_tree_iterator& right) const { return base::operator<=(right); }
        constexpr bool operator>=(const document_tree_iterator& right) const { return base::operator>=(right); }
        
        constexpr std::ptrdiff_t                operator-(const document_tree_iterator& rhs) const { return base::operator-(rhs); }
        constexpr friend document_tree_iterator operator+(const std::ptrdiff_t d, const document_tree_iterator& it) { return it + d; }
    }; 

    template <class StrAllocator = std::allocator<char>, class VecAllocator = std::allocator<tree_node<StrAllocator>>>
    class document_tree {
        std::vector<tree_node<StrAllocator>, VecAllocator> nodes_;
    public:
        using allocator_type         = VecAllocator;
        using value_type             = tree_node<StrAllocator>;
        using pointer                = typename value_type::pointer;
        using const_pointer          = typename value_type::const_pointer;
        using reference              = typename value_type::reference;
        using const_reference        = typename value_type::const_reference;

        using iterator               = document_tree_iterator<document_tree>;
        using const_iterator         = document_tree_const_iterator<document_tree>;
        using reverse_iterator       = std::reverse_iterator<iterator>;
        using const_reverse_iterator = std::reverse_iterator<const_iterator>;

        static constexpr auto upper_bound_rule = [](std::int32_t id, const_reference node) {
            return id < node.parent_id();
        };

        constexpr document_tree(std::string_view name, std::size_t init_cap = 1024, const StrAllocator& str_alloc = StrAllocator{}, const VecAllocator& alloc = VecAllocator{})
            : nodes_(alloc) {
            nodes_.reserve(init_cap);
            nodes_.emplace_back(nullptr, tree_node_category::element, -1 , name, str_alloc);
        }
        
        constexpr decltype(auto)   begin()         { return iterator(this, nodes_.data());        }
        constexpr decltype(auto)   end()           { return iterator(this, nodes_.data() + nodes_.size()); }
        constexpr decltype(auto)   begin()   const { return const_iterator(this, nodes_.data());    }
        constexpr decltype(auto)   end()     const { return const_iterator(this, nodes_.data() + nodes_.size());    }
        constexpr decltype(auto)   cbegin()  const { return begin(); }
        constexpr decltype(auto)   cend()    const { return end();   }
        
        constexpr decltype(auto)   rbegin()        { return reverse_iterator(end()); }
        constexpr decltype(auto)   rend()          { return reverse_iterator(begin()); }
        constexpr decltype(auto)   rbegin()  const { return const_reverse_iterator(end()); }
        constexpr decltype(auto)   rend()    const { return const_reverse_iterator(begin()); }
        constexpr decltype(auto)   crbegin() const { return rbegin(); }
        constexpr decltype(auto)   crend()   const { return rend(); }

        constexpr pointer       data()       { return nodes_.data(); }
        constexpr const_pointer data() const { return nodes_.data(); }
        constexpr std::size_t   size()      const { return nodes_.size(); }
        
        constexpr std::size_t   depth() const {
            return std::ranges::max_element(data(), data() + size(), [](const auto& a, const auto& b) { return a.depth() < b.depth(); })->depth();
        }

        // Always use this if you can.
        constexpr pointer insert(tree_node_category cat, pointer parent, std::string_view node_content, const StrAllocator& alloc = StrAllocator{}) {
            return parent - data() >= nodes_.back().parent_id() ? emplace_node_back_(cat, parent, node_content, alloc) : insert_node_properly_(cat, parent, node_content, alloc);
        }

        // Using binary_search to search the first child node of current.
        constexpr const_pointer search(const_pointer current) const {
            const auto pid = static_cast<std::int32_t>(current - data());
            return std::upper_bound(current, data() + size(), pid - 1, upper_bound_rule);
        }

        // Using binary_search to search the first child node of current.
        constexpr pointer       search(const_pointer current) {
            return const_cast<pointer>(const_cast<const document_tree*>(this)->search(current));
        }

        // Find sibling with rule using linear search.
        template <class Rule>
        constexpr const_pointer find(const_pointer current, Rule rule) const {
            return std::ranges::find_if(current, data() + size(), rule);
        }

        // Find sibling with rule using linear search.
        template <class Rule>
        constexpr pointer       find(pointer current, Rule rule) {
            return const_cast<pointer>(const_cast<const document_tree*>(this)->find(current, rule));
        }

        // // Find nth sibling node.
        // constexpr const_pointer find(const_pointer current, std::size_t n) const {
        //     return find_if(current, finder_by_index_impl{n, current});
        // }
        //
        // // Find nth sibling node.
        // constexpr pointer       find(const_pointer current, std::size_t n) {
        //     return const_cast<pointer>(const_cast<const document_tree*>(this)->find(current, n));
        // }
        //
        // // Find first sibling node with name.
        // constexpr const_pointer find(const_pointer current, std::string_view name) const {
        //     return find_if(current, finder_by_name_impl{ name, current });
        // }
        //
        // // Find first sibling node with name.
        // constexpr pointer       find(const_pointer current, std::string_view name) {
        //     return const_cast<pointer>(const_cast<const document_tree*>(this)->find(current, name));
        // }
        //
        // // Find first sibling node with category 
        // constexpr const_pointer find(const_pointer current, tree_node_category cat) const {
        //     return find_if(current, finder_by_category_impl{cat, current});
        // }
        //
        // // Find first sibling node with category
        // constexpr pointer       find(const_pointer current, tree_node_category cat) {
        //     return const_cast<pointer>(const_cast<const document_tree*>(this)->find(current, cat));
        // }

        ///////////////////////////////////////////////////////////////////////
        ///                       XML Output Part                           ///
        ///////////////////////////////////////////////////////////////////////
        
        template <class OutputIt, class TmpAllocator = std::allocator<char>,
        class DenseAllocator = std::allocator<std::basic_string<char, std::char_traits<char>, TmpAllocator>>>
        constexpr auto    format_to(OutputIt destination, std::size_t max_file_characters = 1 << 24,
            const TmpAllocator& tmp_allocator = TmpAllocator{}, const DenseAllocator& dense_allocator = DenseAllocator{}) const -> OutputIt {
            using temp_string = std::basic_string<char, std::char_traits<char>, TmpAllocator>; 
            // Pre-allocate output space to optimize performance, make sure have enough space.
            temp_string result_cache(max_file_characters, '\0', tmp_allocator);
            // A single root node doesn't require iterations.
            if (depth() == 0) {
                return std::ranges::copy(result_cache.begin(),
                    data()->template format_to<details::document_tag_category::self>(result_cache.begin()), destination).out;
            }
            typename temp_string::iterator cache_it = result_cache.begin();
            std::vector<temp_string, DenseAllocator> node_string_dense(nodes_.size(), dense_allocator);
            for (std::size_t i = depth(); i != 0; --i) {
                auto                     same_depth_view = std::ranges::subrange(data(), data() + size()) | std::views::filter([i](auto& e) { return e.depth() == i; });
                auto                     parent_ids_view = same_depth_view | std::views::transform([](auto& e) { return e.parent_id(); });
                for (auto parent_id : parent_ids_view) {
                    if (node_string_dense[parent_id].empty()) {
                        cache_it = emplace_cache_<details::document_tag_category::open>(nodes_[parent_id], cache_it, i - 1);
                        // Must have this same_parent_view cache or compile won't success.
                        auto same_parent_view = same_depth_view | std::views::filter([parent_id](auto& e) { return e.parent_id() == parent_id; }); 
                        for (auto& j : same_parent_view) {
                            if (node_string_dense[&j - data()].empty()) {
                                cache_it = emplace_cache_<details::document_tag_category::self>(j, cache_it, i);
                            } else {
                                auto& cache = node_string_dense[&j - data()];
                                std::memcpy(&*cache_it, cache.data(), cache.size()); cache_it += cache.size();
                            }
                        }
                        cache_it = emplace_cache_<details::document_tag_category::close>(nodes_[parent_id], cache_it, i - 1);
                        std::construct_at(&node_string_dense[parent_id],result_cache.begin(), cache_it, tmp_allocator);
                        std::memset(result_cache.data(), 0,  cache_it - result_cache.begin()); cache_it = result_cache.begin();
                    }
                }
            }
            return std::ranges::copy(result_cache.begin(),
            std::format_to(result_cache.begin(), R"(<?xml version="1.0" encoding="utf-8"?>)""\n{:s}", node_string_dense.front()), destination).out;
        }

        // API for convenient use.
        constexpr std::string to_string() const {
            std::string str; str.reserve(1 << 24);
            format_to(std::back_inserter(str));
            return str;
        }
        
    private:
        constexpr pointer emplace_node_back_(tree_node_category cat, pointer parent, std::string_view node_content, const StrAllocator& alloc = StrAllocator{}) {
            return &nodes_.emplace_back(data(), cat, static_cast<std::int32_t>(parent - data()), node_content, alloc);
        }

        constexpr pointer insert_node_properly_(tree_node_category cat, pointer parent, std::string_view node_content, const StrAllocator& alloc = StrAllocator{}) {
            const auto pid        = static_cast<std::int32_t>(parent - data());
            const auto insert_pos = static_cast<std::int32_t>(std::upper_bound(data(), data() + size(), pid, upper_bound_rule) - data());
            auto       update_it  = std::upper_bound(data(), data() + size(), static_cast<std::int32_t>(insert_pos - 1), upper_bound_rule);
            std::ranges::for_each(update_it, data() + size(), [](auto& n) { ++n.parent_id(); });
            return &*nodes_.emplace(nodes_.begin() + insert_pos, data(), cat, pid, node_content, alloc);
        }
        
        // To make to_string code more simple and clearer.
        template <details::document_tag_category Cat, class OutputIt>
        static constexpr auto emplace_cache_(const_reference node, OutputIt it, std::size_t depth) -> OutputIt {
            // No newline when outputing text.
            if (node.category() == tree_node_category::text) {
                it = std::prev(it);
            }
            else if (node.category() != tree_node_category::element || Cat != details::document_tag_category::close || *std::prev(it) == '\n') {
                it = std::ranges::fill_n(it, depth << 1, ' ');
            }
            return node.template format_to<Cat>(it);
        }
    };

    ///////////////////////////////////////////////////////////////////////////////////////
    ///                                 XML Input Part                                  ///
    ///////////////////////////////////////////////////////////////////////////////////////

    // Predefine this so we can use it as return value inside document_parser.
    class parser_segment_iterator;

    class document_parser {
        friend class parser_segment_iterator;
        using segment_type = details::segment_context;

        constexpr explicit document_parser() = default;
        
        std::string_view    doc_;
        bool                last_add_one_;
        bool                time_to_add_one_;
        
        constexpr std::string_view skip_decl() {
            auto r = details::parse_tag_decl(doc_, details::no_context);
            if (r) { doc_ = r->second; }
            return r.error();
        }

        // A complete C++20 compiler should make this method constexpr available
        constexpr std::string_view next_segment(segment_type& ctx) {
            // First jump through all spaces.
            if (!time_to_add_one_) {
                doc_ = details::parse_ws(doc_, details::no_context)->second;
                auto r = details::parse_segment(doc_, ctx);
                if (r) { doc_ = r->second; }
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

        template <class StrAllocator, class DocAllocator = std::allocator<char>>
        constexpr parser_segment_iterator to_tree(document_tree<StrAllocator, DocAllocator>&);
        
    };
    
    // A complete C++20 compiler should make all method constexpr available
    class parser_segment_iterator {
        document_parser                   parser_;
        bool                              not_end_ = false;
        std::string_view                  error_;
        document_parser::segment_type     segment_;
    public:
        
        constexpr parser_segment_iterator(document_parser parser) : parser_(parser), not_end_(false), error_(), segment_() {
            if (auto r = parser_.skip_decl(); r.empty()) {
                if (auto s = parser_.next_segment(segment_); s.empty()) {
                    not_end_ = true;
                } else { error_ = s; }
            } else { error_ = r; }
        }

        constexpr parser_segment_iterator(std::string_view e) : parser_(), error_(e), segment_() {}
        constexpr parser_segment_iterator()                                           = default;
        constexpr parser_segment_iterator(const parser_segment_iterator&)             = default;
        constexpr parser_segment_iterator(parser_segment_iterator&&)                  = default;

        parser_segment_iterator& operator=(const parser_segment_iterator&)  = default;
        parser_segment_iterator& operator=(parser_segment_iterator&&)       = default;

        constexpr bool operator==(const parser_segment_iterator& other) const { return not_end_ == other.not_end_ && parser_.doc_ == other.parser_.doc_; }
        constexpr bool operator!=(const parser_segment_iterator& other) const { return not_end_ != other.not_end_ || parser_.doc_ != other.parser_.doc_; }

        constexpr auto operator*()  const       { return segment_; }
        constexpr auto operator->() const   { return &segment_; }

        constexpr parser_segment_iterator& operator++() {
            segment_.buffer.clear();
            error_   = parser_.next_segment(segment_);
            not_end_ = parser_.not_end();
            return *this;
        }
        
        constexpr parser_segment_iterator  operator++(int) { auto tmp = *this; ++*this; return tmp; }

        constexpr operator bool()          const noexcept { return error_.empty(); }
        constexpr std::string_view error() const noexcept { return error_; }
    };

    //////////////////////////////////////////////////////////////////////////
    ///                         XML Input Part                             ///
    //////////////////////////////////////////////////////////////////////////
    
    template <class StrAllocator, class DocAllocator>
    constexpr parser_segment_iterator document_parser::to_tree(document_tree<StrAllocator, DocAllocator>& tree_mem) {
        // Cache the index inside tree, an elegant way to avoid recursive. "inspired by Dijkstra's Double Stack Algorithm"
        std::uint8_t                  stack_top = 0;
        std::int32_t                  segment_stack[128]; // I think this is already large enough.
        parser_segment_iterator       current_segment(*this);
        
        if (current_segment->category != details::document_tag_category::open && current_segment->category != details::document_tag_category::self) {
            return parser_segment_iterator("Root node (first node) can not be comment or anything else!");
        }
        // Add root first because our tree doesn't support pushing root after construction.
        tree_mem.data()->buffer(current_segment->buffer);
        segment_stack[++stack_top] = 0;
        
        for (++current_segment; current_segment != parser_segment_iterator(); ++current_segment) {
            if (!current_segment) {
                return parser_segment_iterator(current_segment.error());
            }
            switch (current_segment->category) {
            case details::document_tag_category::text:
                tree_mem.insert(tree_node_category::text, tree_mem.data() + segment_stack[stack_top], current_segment->buffer, tree_mem.data()->get_allocator()); break;
            case details::document_tag_category::comment: break;
            case details::document_tag_category::open:
                segment_stack[++stack_top] = static_cast<std::int32_t>(
                    tree_mem.insert(tree_node_category::element, tree_mem.data() + segment_stack[stack_top],
                   current_segment->buffer, tree_mem.data()->get_allocator()) - tree_mem.data()); break;
            case details::document_tag_category::close:
                if (current_segment->buffer != (tree_mem.data() + segment_stack[stack_top])->name()) {
                    return parser_segment_iterator("Unmatched element!"); 
                } --stack_top; break;
            case details::document_tag_category::self:
                tree_mem.insert(tree_node_category::element, tree_mem.data() + segment_stack[stack_top],
                    current_segment->buffer, tree_mem.data()->get_allocator()); break;
            case details::document_tag_category::unknow:
                return parser_segment_iterator("Not a valid segment type");
            }
        }
        return current_segment;
    }

}