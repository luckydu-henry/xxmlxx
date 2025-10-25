//
// MIT License
//
// Copyright (c) 2025 Henry Du
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
#pragma once

#include <format>
#include <ranges>
#include <string>
#include <vector>
#include <cstdint>
#include <utility>
#include <variant>
#include <optional>
#include <algorithm>

namespace xxmlxx {
    
    // No entity(reference) because entity will be included in string and be handled as escape characters
    enum class node_type : std::uint8_t {
        unknown         = 0,
        comment         = 1,
        element         = 2,
        attribute       = 3,
        text            = 4,
        document        = 5,
        declaration     = 6
    };

    template <class DomTree>
    class document_tree_node_const_iterator;

    template <class DomTree>
    class document_tree_node_iterator;

    template <class BufferAllocator = std::allocator<char>>
    class document_tree_node final {
    public:
        using allocator_type      = BufferAllocator;
        using buffer_type         = std::basic_string<char, std::char_traits<char>, BufferAllocator>;
        using pointer             = document_tree_node*;
        using const_pointer       = const document_tree_node*;
        using reference           = document_tree_node&;
        using const_reference     = const document_tree_node&;
        using difference_type     = std::ptrdiff_t;
    private:
        node_type           cached_type_;
    public:
        node_type           type         = node_type::unknown;
        buffer_type         data;       // All node data is inside this 'value buffer', this is the core to avoid inheritance.
        difference_type     parent_index = -1;
        
        constexpr document_tree_node& dying(bool d) {
            if ((d && type != node_type::unknown) || (!d && type == node_type::unknown)) { std::swap(cached_type_, type); }
            return *this;
        }
        constexpr bool           dying() const { return type == node_type::unknown; }

        constexpr decltype(auto) get_allocator() const { return data.get_allocator(); }

        constexpr document_tree_node(pointer parent, pointer begin, node_type asked_type, std::string_view node_content, const BufferAllocator& allocator)
        : cached_type_(node_type::unknown), type(asked_type), data(allocator), parent_index(parent != nullptr ? parent - begin : -1) {
            data.reserve(256);
            data.assign(node_content);
        }

        // template <class DomTree>
        // constexpr dom_node_data(node_type asked_type, std::string_view node_content, dom_tree_node_const_iterator<DomTree> nit);

        constexpr document_tree_node()                                    = default;
        constexpr document_tree_node(const document_tree_node&)                = default;
        constexpr document_tree_node(document_tree_node&&)                     = default;
        constexpr document_tree_node& operator=(const document_tree_node&)     = default;
        constexpr document_tree_node& operator=(document_tree_node&&)          = default;
        constexpr ~document_tree_node()                                   = default;

        // Useful when replacing nodes.
        constexpr void swap(document_tree_node& rhs) noexcept {
            std::swap(cached_type_, rhs.cached_type_);
            std::swap(type, rhs.type);
            std::swap(data, rhs.data);
            std::swap(parent_index, rhs.parent_index);
        }

        // Common for all node types.
        constexpr std::string_view name() const {
            using namespace std::string_view_literals;
            switch (type) {
            case node_type::unknown: break;
            case node_type::comment:            return "#comment"sv;
            case node_type::document:           return "#document"sv;
            case node_type::declaration:        return "#declaration"sv;
            case node_type::element:
            case node_type::text:               return data;
            case node_type::attribute:          return std::string_view(data.data(), data.find('='));
            }
            return ""sv;
        }

        constexpr std::string_view value() const {
            if (type == node_type::attribute) {
                return std::string_view(data).substr(data.find('=') + 1);
            }
            return data;
        }

        constexpr void             value(std::string_view value) {
            if (type == node_type::attribute) {
                data = name();
                data.push_back('=');
                data += value;
            } else {
                data = value;
            }
        }

        template <class Ty> requires std::is_standard_layout_v<Ty>
        constexpr void             value(const Ty& value) {
            if (type == node_type::attribute) {
                data = name();
                data.push_back('=');
                data.append(reinterpret_cast<const char*>(&value), sizeof(Ty));
            } else  {
                data.clear();
                data.append(reinterpret_cast<const char*>(&value), sizeof(Ty));
            }
        }

        template <class Ty> requires std::is_standard_layout_v<Ty>
        constexpr Ty&               value() {
            if (type == node_type::attribute) {
                return *reinterpret_cast<Ty*>(const_cast<char*>(data.data() + (data.find('=') + 1)));
            } 
            return *reinterpret_cast<Ty*>(const_cast<char*>(data.data()));
        }

        template <class Ty> requires std::is_standard_layout_v<Ty>
        constexpr Ty                value() const {
            if (type == node_type::attribute) {
                return *reinterpret_cast<Ty*>(const_cast<char*>(data.data() + (data.find('=') + 1)));
            } 
            return *reinterpret_cast<Ty*>(const_cast<char*>(data.data()));
        }
        
    };
    

    // These two iterators are that node api in MDN specification.
    template <class DomTree>
    class document_tree_node_const_iterator {
        public:
        using tree_type         = DomTree;
        using difference_type   = std::ptrdiff_t;
        using iterator_category = std::random_access_iterator_tag;
        using iterator_concept  = std::contiguous_iterator_tag;
        
        using value_type         = typename tree_type::value_type;
        using pointer            = typename tree_type::const_pointer;
        using reference          = typename tree_type::const_reference;
        using container_iterator = typename tree_type::container_const_iterator;
    protected:
        pointer       node_ptr_ = nullptr;
        tree_type*    tree_ptr_ = nullptr;
    public:
        ~document_tree_node_const_iterator() = default;
        constexpr document_tree_node_const_iterator(const document_tree_node_const_iterator&) = default;
        constexpr document_tree_node_const_iterator(document_tree_node_const_iterator&&) = default;
        constexpr document_tree_node_const_iterator() = default;
        
        constexpr document_tree_node_const_iterator(const tree_type* const tp, const pointer np)
            : node_ptr_(const_cast<pointer>(np)), tree_ptr_(const_cast<tree_type*>(tp)) {}

        constexpr document_tree_node_const_iterator& operator=(const document_tree_node_const_iterator&) = default;
        constexpr document_tree_node_const_iterator& operator=(document_tree_node_const_iterator&&)      = default;

        constexpr reference       operator*()               const { return *node_ptr_; }
        constexpr pointer         operator->()              const { return node_ptr_; }
        constexpr reference       operator[](std::size_t i) const { return node_ptr_[i]; }

        constexpr document_tree_node_const_iterator& operator++()                             { ++node_ptr_; return *this; }
        constexpr document_tree_node_const_iterator  operator++(int)                          { return document_tree_node_const_iterator( tree_ptr_, node_ptr_++); }
        constexpr document_tree_node_const_iterator& operator+=(const std::ptrdiff_t d)       { node_ptr_ += d; return *this; }
        constexpr document_tree_node_const_iterator  operator+ (const std::ptrdiff_t d) const { return document_tree_node_const_iterator(tree_ptr_, node_ptr_ + d); }

        constexpr document_tree_node_const_iterator& operator--()                             { --node_ptr_; return *this; }
        constexpr document_tree_node_const_iterator  operator--(int)                          { return document_tree_node_const_iterator(tree_ptr_, node_ptr_--); }
        constexpr document_tree_node_const_iterator& operator-=(const std::ptrdiff_t d)       { node_ptr_ -= d; return *this; }
        constexpr document_tree_node_const_iterator  operator- (const std::ptrdiff_t d) const { return document_tree_node_const_iterator(tree_ptr_, node_ptr_ - d); }
        
        constexpr bool operator==(const document_tree_node_const_iterator& right) const { return tree_ptr_ == right.tree_ptr_ && node_ptr_ == right.node_ptr_; }
        constexpr bool operator!=(const document_tree_node_const_iterator& right) const { return node_ptr_ != right.node_ptr_ || tree_ptr_ != right.tree_ptr_; }
        constexpr bool operator< (const document_tree_node_const_iterator& right) const { return tree_ptr_ == right.tree_ptr_ && node_ptr_ <  right.node_ptr_; }
        constexpr bool operator> (const document_tree_node_const_iterator& right) const { return tree_ptr_ == right.tree_ptr_ && node_ptr_ >  right.node_ptr_; }
        constexpr bool operator<=(const document_tree_node_const_iterator& right) const { return tree_ptr_ == right.tree_ptr_ && node_ptr_ <= right.node_ptr_; }
        constexpr bool operator>=(const document_tree_node_const_iterator& right) const { return tree_ptr_ == right.tree_ptr_ && node_ptr_ >= right.node_ptr_; }

        constexpr std::ptrdiff_t                      operator-(const document_tree_node_const_iterator& rhs) const { return node_ptr_ - rhs.node_ptr_; }
        constexpr friend document_tree_node_const_iterator operator+(const std::ptrdiff_t d, const document_tree_node_const_iterator& it) { return it + d; }

        constexpr document_tree_node_const_iterator parent()                        const {
            return node_ptr_->parent_index != -1 ? document_tree_node_const_iterator(tree_ptr_, tree_ptr_->data() + node_ptr_->parent_index) : *this;
        }
        constexpr document_tree_node_const_iterator child_begin()                   const {
            return tree_ptr_->search_child_begin(*this);
        }
        constexpr document_tree_node_const_iterator child_end()                     const {
            return tree_ptr_->search_child_end(*this);
        }
        constexpr document_tree_node_const_iterator child_rbegin()                  const {
            return child_end() - 1;
        }
        constexpr document_tree_node_const_iterator child_rend()                    const {
            return child_begin() - 1;
        }
        // Next not unknow node.
        constexpr document_tree_node_const_iterator sibling_next()                          const {
            auto it = *this;
            for (++it; it != parent().child_end() && it->type == node_type::unknown; ++it) {}
            return it;
        }

        constexpr document_tree_node_const_iterator sibling_prev()                          const {
            auto it = *this;
            for (--it; it != parent().child_rend() && it->type == node_type::unknown; --it) {}
            return it;
        }
        
    };

    template <class DomTree>
    class document_tree_node_iterator : protected document_tree_node_const_iterator<DomTree> {
        using base = document_tree_node_const_iterator<DomTree>;
        constexpr document_tree_node_iterator(const base& right) : base(right) {}
    public:
        using tree_type         = typename base::tree_type;
        using difference_type   = typename base::difference_type;
        using iterator_category = typename base::iterator_category;
        using iterator_concept  = typename base::iterator_concept;
        
        using value_type          = typename tree_type::value_type;
        using pointer             = typename tree_type::pointer;
        using reference           = typename tree_type::reference;
        using container_iterator  = typename tree_type::container_iterator;
    protected:
        using base::node_ptr_;
        using base::tree_ptr_;
    public:
        ~document_tree_node_iterator() = default;
        constexpr document_tree_node_iterator(const document_tree_node_iterator&) = default;
        constexpr document_tree_node_iterator(document_tree_node_iterator&&)      = default;
        constexpr document_tree_node_iterator() = default;
        
        constexpr document_tree_node_iterator(const tree_type* const tp, const pointer np)            : base(tp, np) {}

        constexpr document_tree_node_iterator& operator=(const document_tree_node_iterator&) = default;
        constexpr document_tree_node_iterator& operator=(document_tree_node_iterator&&)      = default;

        constexpr reference       operator*()               const { return const_cast<reference>(base::operator*()); }
        constexpr pointer         operator->()              const { return const_cast<pointer>  (base::operator->()); }
        constexpr reference       operator[](std::size_t i) const { return const_cast<reference>(base::operator[](i)); }

        constexpr document_tree_node_iterator& operator++()                             { base::operator++(); return *this; }
        constexpr document_tree_node_iterator  operator++(int)                          { return base::operator++(0); }
        constexpr document_tree_node_iterator& operator+=(const std::ptrdiff_t d)       { base::operator+=(d); return *this; }
        constexpr document_tree_node_iterator  operator+ (const std::ptrdiff_t d) const { return base::operator+ (d); }

        constexpr document_tree_node_iterator& operator--()                             { base::operator--(); return *this;  }
        constexpr document_tree_node_iterator  operator--(int)                          { return base::operator--(0); }
        constexpr document_tree_node_iterator& operator-=(const std::ptrdiff_t d)       { base::operator-=(d); return *this; }
        constexpr document_tree_node_iterator  operator- (const std::ptrdiff_t d) const { return base::operator- (d); }

        constexpr bool operator==(const document_tree_node_iterator& right) const { return base::operator==(right); }
        constexpr bool operator!=(const document_tree_node_iterator& right) const { return base::operator!=(right); }
        constexpr bool operator< (const document_tree_node_iterator& right) const { return base::operator<(right); }
        constexpr bool operator> (const document_tree_node_iterator& right) const { return base::operator>(right); }
        constexpr bool operator<=(const document_tree_node_iterator& right) const { return base::operator<=(right); }
        constexpr bool operator>=(const document_tree_node_iterator& right) const { return base::operator>=(right); }
        
        constexpr std::ptrdiff_t                operator-(const document_tree_node_iterator& rhs) const { return base::operator-(rhs); }
        constexpr friend document_tree_node_iterator operator+(const std::ptrdiff_t d, const document_tree_node_iterator& it) { return it + d; }

        constexpr document_tree_node_iterator parent()                  const { return base::parent(); }
        
        constexpr document_tree_node_iterator child_begin()             const { return base::child_begin();  }
        constexpr document_tree_node_iterator child_end()               const { return base::child_end();    }
        constexpr document_tree_node_iterator child_rbegin()            const { return base::child_rbegin(); }
        constexpr document_tree_node_iterator child_rend()              const { return base::child_rend(); }
        constexpr document_tree_node_iterator sibling_next()            const { return base::sibling_next(); }
        constexpr document_tree_node_iterator sibling_prev()            const { return base::sibling_prev(); }

        constexpr document_tree_node_iterator emplace(node_type type, std::string_view content,
            const typename value_type::allocator_type& allocator = typename value_type::allocator_type{}) {
            return tree_ptr_->emplace(type, *this, content, allocator);
        }

        constexpr document_tree_node_iterator emplace_before(document_tree_node_iterator which, node_type type, std::string_view content,
            const typename value_type::allocator_type& allocator = typename value_type::allocator_type{}) {
            return tree_ptr_->emplace_before(type, *this, which, content, allocator);
        }

        constexpr document_tree_node_iterator remove() {
            tree_ptr_->remove(*this);
            return tree_ptr_->begin();
        }
    };

    template <class BufferAllocator = std::allocator<char>, class TreeAllocator = std::allocator<document_tree_node<BufferAllocator>>>
    class document_tree {
    public:
        using allocator_type    = TreeAllocator;
        using value_type        = document_tree_node<BufferAllocator>;
        using reference         = value_type&;
        using const_reference   = const value_type&;
        using pointer           = value_type*;
        using const_pointer     = const value_type*;
        using difference_type   = std::ptrdiff_t;

        using container                        = std::vector<value_type, allocator_type>;
        using container_iterator               = typename container::iterator;
        using container_const_iterator         = typename container::const_iterator;
        using container_reverse_iterator       = typename container::reverse_iterator;
        using container_const_reverse_iterator = typename container::const_reverse_iterator;

        using iterator               = document_tree_node_iterator<document_tree>;
        using const_iterator         = document_tree_node_const_iterator<document_tree>;
        using reverse_iterator       = std::reverse_iterator<iterator>;
        using const_reverse_iterator = std::reverse_iterator<const_iterator>;
    protected:
        container       nodes_;
        
        static constexpr auto upper_bound_proj = [](const value_type& v) { return v.parent_index; };

        template <class ... Args>
        constexpr document_tree(allocator_type alloc, std::size_t init_cap, pointer parent, Args&& ... args) : nodes_(alloc) {
            nodes_.reserve(init_cap);
            emplace_back_(parent, std::forward<Args>(args)...);
        }

        template <class ... Args>
        constexpr container_iterator emplace_back_(pointer parent, Args&& ... args) {
            return nodes_.emplace(nodes_.end(), parent, std::forward<Args>(args)...);
        }
        
        template <class ... Args>
        constexpr container_iterator emplace_sorted_(pointer parent, Args&& ... args) {
            auto insert_pos = std::ranges::upper_bound(nodes_, parent - data(), std::less<difference_type>(), upper_bound_proj) - nodes_.begin();
            auto update_itr = std::ranges::upper_bound(nodes_, insert_pos - 1 , std::less<difference_type>(), upper_bound_proj);
            std::ranges::for_each(update_itr, nodes_.end(), [](value_type& nd) { ++nd.parent_index; });
            return nodes_.emplace(nodes_.begin() + insert_pos, parent, std::forward<Args>(args)...);
        }
        
        template <class ... Args>
        constexpr container_iterator emplace_auto_(pointer parent, Args&& ... args) {
            return parent - data() >= nodes_.back().parent_index ?
            emplace_back_(parent, std::forward<Args>(args)...) : emplace_sorted_(parent, std::forward<Args>(args)...);
        }
        
        template <class ... Args>
        constexpr container_iterator emplace_at_auto_(pointer parent, pointer at, Args&& ... args) {
            if (parent - data() < at->parent_index) {
                return emplace_auto_(parent, std::forward<Args>(args)...);
            }
            if (parent - data() == at->parent_index) {
                auto insert_pos = at - data();
                auto update_itr = std::ranges::upper_bound(nodes_, insert_pos - 1 , std::less<difference_type>(), upper_bound_proj);
                std::ranges::for_each(update_itr, nodes_.end(), [](value_type& nd) { ++nd.parent_index; });
                return nodes_.emplace(nodes_.begin() + insert_pos, parent, std::forward<Args>(args)...);
            }
            return nodes_.end();
        }

        constexpr void               tag_current_and_all_children_to_unknow_(container_iterator root) {
            if (root == nodes_.end()) {
                return;
            }
            root->dying(true);
            auto beg = std::ranges::upper_bound(root, nodes_.end(), root - nodes_.begin() - 1, std::less<difference_type>(), upper_bound_proj);
            auto end = std::ranges::upper_bound(root, nodes_.end(), root - nodes_.begin(),     std::less<difference_type>(), upper_bound_proj);
            for (;beg != end; ++beg) {
                tag_current_and_all_children_to_unknow_(beg);
            }
        }

        constexpr container_iterator erase_single_node_and_rotate_(container_iterator which) {
            auto dbg = std::ranges::upper_bound(nodes_, which - nodes_.begin() - 1, std::less<difference_type>(), upper_bound_proj);
            std::ranges::for_each(dbg, nodes_.end(), [](value_type& n) { --n.parent_index; });
            return nodes_.erase(which);
        }

        constexpr container_iterator erase_all_unknows_(container_iterator from) {
            for (;from != nodes_.end(); ++from) {
                if (from->dying()) {  from = erase_single_node_and_rotate_(from); --from; }
            }
            return from;
        }
    public:

        constexpr document_tree(std::size_t init_cap = 1024, const BufferAllocator& buf_alloc = BufferAllocator{}, const TreeAllocator& tree_alloc = TreeAllocator{})
        : nodes_(tree_alloc) {
            nodes_.reserve(init_cap);
            emplace_back_(nullptr, data(), node_type::document, "", buf_alloc);
        }
        
        constexpr std::size_t        size() const noexcept { return nodes_.size(); }
        constexpr pointer            data()       noexcept { return nodes_.data(); }
        constexpr const_pointer      data() const noexcept { return nodes_.data(); }

        constexpr decltype(auto)     begin()         { return iterator(this, data());                         }
        constexpr decltype(auto)     end()           { return iterator(this, data() + size());                }
        constexpr decltype(auto)     begin()   const { return const_iterator(this, data());                   }
        constexpr decltype(auto)     end()     const { return const_iterator(this, data() + size());          }
        constexpr decltype(auto)     cbegin()  const { return const_iterator(this, data());                   }
        constexpr decltype(auto)     cend()    const { return const_iterator(this, data() + size());          }
        constexpr decltype(auto)     rbegin()        { return reverse_iterator(end());                        }
        constexpr decltype(auto)     rend()          { return reverse_iterator(begin());                      }
        constexpr decltype(auto)     rbegin()  const { return const_reverse_iterator(end());                  }
        constexpr decltype(auto)     rend()    const { return const_reverse_iterator(begin());                }
        constexpr decltype(auto)     crbegin() const { return rbegin();                                       }
        constexpr decltype(auto)     crend()   const { return rend();                                         }

        constexpr iterator emplace(node_type type, iterator parent, std::string_view content, const BufferAllocator& allocator = BufferAllocator{}) {
            return iterator(this, &*emplace_auto_(&*parent, data(), type, content, allocator));
        }

        constexpr iterator emplace_before(node_type type, iterator parent, iterator at, std::string_view content, const BufferAllocator& allocator = BufferAllocator{}) {
            auto it = emplace_at_auto_(&*parent, &*at, data(), type, content, allocator);
            return iterator(this, it != nodes_.end() ? &*it : (data() + size()));
        }

        constexpr const_iterator search_child_begin(const_iterator parent) const {
            auto it = std::ranges::upper_bound(nodes_.begin() + (parent - begin()), nodes_.end(), parent - begin() - 1, std::less<difference_type>(), upper_bound_proj);
            return const_iterator(this, it != nodes_.end() ? &*it : (data() + size()));
        }

        constexpr const_iterator search_child_end(const_iterator parent) const {
            auto it = std::ranges::upper_bound(nodes_.begin() + (parent - begin()), nodes_.end(), parent - begin(), std::less<difference_type>(), upper_bound_proj);
            return const_iterator(this, it != nodes_.end() ? &*it : (data() + size()));
        }

        constexpr void           remove(iterator which) {
            tag_current_and_all_children_to_unknow_(nodes_.begin() + (which - begin()));
        }

        constexpr iterator       erase(iterator from) {
            auto it = erase_all_unknows_(nodes_.begin() + (from - begin()));
            return iterator(this, it != nodes_.end() ? &*it : (data() + size()));
        }
        
    };

    ///////////////////////////////////////////////////////////////////////////////////////////
    ///                              XML Parsing Part                                       ///
    ///////////////////////////////////////////////////////////////////////////////////////////

    namespace parser {

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
        static constexpr auto operator,(const P1& p1, const P2& p2) { return parser_continuation_impl<P1, P2>{p1, p2}; }

        template <parser P1, parser P2>
        static constexpr auto operator|(const P1& p1, const P2& p2) { return parser_variation_impl<P1,P2>{p1, p2}; }

        template <parser Parser>
        static constexpr auto operator~(const Parser& p) { return parser_optional_impl<Parser>{p}; }
        
        template <parser Parser, class Action>
        static constexpr auto operator%(const Parser& p, Action act) { return parser_action_impl<Parser, Action>{p, act}; }

        template <bool Inversed>
        static constexpr auto operator!(const parser_string_range_character_impl<Inversed>& p) {
            return parser_string_range_character_impl<!Inversed>{p.range};
        }

        template <bool Inversed>
        static constexpr auto operator!(const parser_string_range_string_impl<Inversed>& p) {
            return parser_string_range_string_impl<!Inversed>{p.range};
        }

        // lot_of string operator.
        static constexpr auto operator""_ls(const char* input, std::size_t size) {
            return parser_string_range_string_impl<false>{std::string_view(input, size)};
        }

        // one_of string operator.
        static constexpr auto operator""_os(const char* input, std::size_t size) {
            return parser_string_range_character_impl<false>{std::string_view(input, size)}; 
        }

        // sequence is string operator.
        static constexpr auto operator""_ss(const char* input, std::size_t size) {
            return parser_string_impl{std::string_view(input, size)};
        }
        
        static constexpr int  no_context = 0;

        enum class xml_parse_flag : std::uint8_t {
            empty = 0,
            is_element_begin_left,
            is_element_begin_right,
            is_element_self_closing,
            is_element_end,
            is_previous_text,
            is_first_text
        };

        struct parser_context {
            node_type        type = node_type::unknown;
            xml_parse_flag   flag = xml_parse_flag::empty;
            std::string      buffer;
            constexpr parser_context() { buffer.reserve(2048); }
        };

        // May be replaced by a map
        constexpr std::string acquire_escape_replacement(std::string_view input) {
            // Now only support these basic entities.
            if (input == "quot") { return "\""; }
            if (input == "apos") { return "'";  }
            if (input == "amp")  { return "&";  }
            if (input == "lt")   { return "<";  }
            if (input == "gt")   { return ">";  }
            if (input == "nbsp") { return " ";  }
            // This is for byte data.
            if (input[0] == '#') {
                input = input.substr(1);
                std::uint8_t index = 0;
                // Fast conversion.
                for (auto& i : input) {
                    index *= 10;
                    index += (i - '0');
                }
                // SSO makes return value in this case much faster.
                return std::string(reinterpret_cast<const char*>(&index), 1);
            }
            return "";
        }
        
        constexpr void replace_text_entities(std::string& text, std::size_t beg, std::size_t end) {
            for (std::size_t i = beg; i != end; ++i) {
                if (text[i] == '&') {
                    const std::size_t check_start = text.find_first_not_of('&', i);
                    if (std::size_t check_end = text.find(';', check_start); check_end != std::string::npos) {
                        const std::size_t escape_length = check_end - check_start;
                        std::string_view  escape_sequence(text.data() + check_start, escape_length);
                        std::string       replacement = acquire_escape_replacement(escape_sequence);
                        text.replace(check_start - 1, escape_length + 2, replacement);
                        end += replacement.length() - escape_length -2;
                    }
                }
            }
        }

#define CONTEXT() [](auto&& input, auto&& result, parser_context& ctx)

        static constexpr auto xml_parse_name_chars    =  !" \n\t\r/<>=\"\'"_ls;
        static constexpr auto xml_parse_name_char     = !" \n\t\r/<>=\"\'"_os;
        static constexpr auto xml_parse_space_chars   = " \n\t\r"_ls;
        static constexpr auto xml_parse_space_char    = " \n\t\r"_os;
        static constexpr auto xml_parse_attribute_value = ("\""_ss, !"\""_ls, "\""_ss) | ("'"_ss, !"'"_ls, "'"_ss); 
        
        static constexpr auto xml_parse_comment                              = ("<!--"_ss, ~"-"_ss, !"-"_ls, "-->"_ss) % CONTEXT() {
            ctx.type = node_type::comment;
            ctx.flag = xml_parse_flag::empty;
        };
        
        static constexpr auto xml_parse_declaration       = (R"(<?xml version="1.0" encoding="utf-8"?>)"_ss) % CONTEXT() {
            ctx.type   = node_type::declaration;
            ctx.buffer.assign(R"(version="1.0" encoding="utf-8")");
            ctx.flag   = xml_parse_flag::empty;
        };
        
        static constexpr auto xml_parse_element_begin_left    = ("<"_ss, xml_parse_name_char, xml_parse_name_chars) % CONTEXT() {
            ctx.type = node_type::element;
            ctx.flag = xml_parse_flag::is_element_begin_left;
            ctx.buffer.append(std::get<1>(result->first)).append(std::get<2>(result->first));
        };
        
        static constexpr auto xml_parse_element_begin_right   = (~"/"_ss, ">"_ss) % CONTEXT() {
            ctx.type = node_type::element;
            ctx.flag = std::get<0>(result->first) ? xml_parse_flag::is_element_self_closing : xml_parse_flag::is_element_begin_right;
        };
        
        static constexpr auto xml_parse_element_end = ("</"_ss, xml_parse_name_chars, ">"_ss) % CONTEXT() {
            ctx.type = node_type::element;
            ctx.flag = xml_parse_flag::is_element_end;
            ctx.buffer.assign(std::get<1>(result->first));
        };
        
        static constexpr auto xml_parse_attribute = (xml_parse_name_char, xml_parse_name_chars, xml_parse_space_chars, "="_ss, xml_parse_space_chars, xml_parse_attribute_value) % CONTEXT() {
            auto value_variant = std::get<5>(result->first);
            ctx.type = node_type::attribute;
            ctx.flag = xml_parse_flag::empty;
            ctx.buffer.append(std::get<0>(result->first)).append(std::get<1>(result->first)).push_back('=');
            const std::size_t replace_beg = ctx.buffer.size();
            ctx.buffer.append(std::visit([](auto&& value) { return std::get<1>(value); }, value_variant));
            const std::size_t replace_end = ctx.buffer.size();
            replace_text_entities(ctx.buffer, replace_beg, replace_end);
        };
        
        static constexpr auto xml_parse_text = (!" \n\t\r<>=\"\'"_os, !" \n\t\r<>=\"\'"_ls) % CONTEXT() {
            ctx.type = node_type::text;
            ctx.flag = (ctx.flag != xml_parse_flag::is_first_text && ctx.flag != xml_parse_flag::is_previous_text) ?
                       xml_parse_flag::is_first_text : xml_parse_flag::is_previous_text;
            ctx.buffer.append(std::get<0>(result->first)).append(std::get<1>(result->first));
            replace_text_entities(ctx.buffer, 0, ctx.buffer.size());
        };
        
        static constexpr auto xml_parse_token = xml_parse_declaration | xml_parse_comment | xml_parse_element_end | xml_parse_element_begin_left | xml_parse_element_begin_right |
            xml_parse_attribute | xml_parse_text;
#undef  CONTEXT
    }

     class document_parser {
        constexpr std::string_view parse_token_to_context(parser::parser_context& ctx) {
            doc = parser::xml_parse_space_chars(doc, parser::no_context)->second;
            if (!doc.empty()) {
                auto r = parser::xml_parse_token(doc, ctx);
                if (r) { doc = r->second; }
                return r.error();
            }
            ctx.type = node_type::unknown;
            return "";
        }
    public:
        std::string_view doc;
        template <class BufferAllocator, class TreeAllocator = std::allocator<char>>
        constexpr std::string_view  operator()(document_tree<BufferAllocator, TreeAllocator>& tree) {
            std::uint8_t                  stack_top = 0;
            std::ptrdiff_t                segment_stack[255]; // I think this is already large enough.
            std::string_view              error;
            parser::parser_context        context;
            typename document_tree<BufferAllocator, TreeAllocator>::iterator current_it = tree.begin();
            segment_stack[++stack_top] = 0;
            do {
                error = parse_token_to_context(context);
                if (!error.empty()) {
                    return error;
                }
                switch (context.type) {
                case node_type::unknown:
                case node_type::comment: break;
                case node_type::document:
                    return "Error parsing HTML: only root node can be document node.";
                case node_type::element:
                    switch (context.flag) {
                    case parser::xml_parse_flag::is_element_begin_left:
                        current_it = tree.emplace(context.type, tree.begin() + segment_stack[stack_top], context.buffer, tree.data()->get_allocator()); 
                        segment_stack[++stack_top] = current_it - tree.begin(); break;
                    case parser::xml_parse_flag::is_element_self_closing: --stack_top; break;
                    case parser::xml_parse_flag::is_element_end:
                        if (context.buffer != (tree.begin() + segment_stack[stack_top])->name()) {
                            return "Error parsing HTML, error unmatched element or maybe forget to add / to self close!";
                        } --stack_top; break;
                    case parser::xml_parse_flag::empty:
                    case parser::xml_parse_flag::is_element_begin_right:
                    case parser::xml_parse_flag::is_previous_text:
                    case parser::xml_parse_flag::is_first_text: break;
                    }
                    break;
                case node_type::attribute:
                case node_type::declaration:
                    current_it = tree.emplace(context.type, tree.begin() + segment_stack[stack_top], context.buffer, tree.data()->get_allocator()); break;
                case node_type::text:
                    // Special insert to combine text segment to one.
                    if (context.flag == parser::xml_parse_flag::is_first_text) {
                        current_it = tree.emplace(context.type, tree.begin() + segment_stack[stack_top], context.buffer, tree.data()->get_allocator());
                        break;
                    }
                    if (context.flag == parser::xml_parse_flag::is_previous_text) {
                        current_it->data.push_back(' ');
                        current_it->data.append(context.buffer);
                        break;
                    }
                    break;
                }
                context.buffer.clear();
            } while (!doc.empty());
            return error;
        }
    };
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///                                                         MAIN CONTRIBUTOR LIST                                                                    ///
///                                  AFTER YOU HAVE CONTRIBUTED TO THIS PROJECT YOUR NAME WOULD BE SHOWN HERE!                                       ///                                    
///                                                                                                                                                  ///
///                     xflcx1991@<https://github.com/xflcx1991>: Ported v1 version of this library to xmake package.                                ///
///                                                                                                                                                  ///
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////                     