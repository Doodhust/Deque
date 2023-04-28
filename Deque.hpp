#pragma once
#include <iostream>
#include <limits>

namespace fefu_laboratory_two {
    template <typename T>
    class Allocator {
    public:
        using value_type = T;
        using size_type = std::size_t;
        using difference_type = std::ptrdiff_t; // знаковый тип для хранения разности двух указателей
        using pointer = T*;
        using const_pointer = const T*;
        using reference = T&;
        using const_reference = const T&;

        Allocator() = default;

        Allocator(const Allocator& other) = default;

        template <class U>
        Allocator(const Allocator<U>& other){}

        ~Allocator() = default;

        pointer allocate(size_type n)
        {
            return static_cast<pointer>(operator new(sizeof(T) * n ));
        }

        void deallocate(pointer p) noexcept // спецификатор времени компиляции(говорим что не будет исключений)
        {
            operator delete(p);
        }

        //[[nodiscard]] std::allocation_result<T*> allocate_at_least(
        // std::size_t n ); // TODO For extra points
    };

    template <typename T>
    class Node {
    public:
        T value = NULL;
        Node* next = nullptr;
        Node* previous = nullptr;
    };

    template <typename ValueType>
    class Deque_iterator {
    public:
        using iterator_category = std::random_access_iterator_tag;
        using value_type = ValueType;
        using difference_type = std::ptrdiff_t;
        using pointer = ValueType*;
        using reference = ValueType&;
        Node<value_type> *cur;
        Deque_iterator() = default;
        Deque_iterator(pointer p){
            cur = p;
        }
        Deque_iterator(const Deque_iterator& other) noexcept{
            cur = other.cur;
        }


        Deque_iterator& operator=(const Deque_iterator& a)
        {
            this -> cur = a.cur;
            return *this;
        }

        Deque_iterator& operator=(const Deque_iterator&& a)
        {
            cur = std::move(a.cur);
            return *this;
        }

        ~Deque_iterator() = default;

        friend void swap(Deque_iterator<ValueType>& a, Deque_iterator<ValueType>& b)
        {
            Deque_iterator<ValueType> c = std::move(a);
            a = std::move(b);
            b = std::move(c);
        }

        friend bool operator==(const Deque_iterator<ValueType>& a,
                               const Deque_iterator<ValueType>& b)
        {
            return a.cur == b.cur;
        }
        friend bool operator!=(const Deque_iterator<ValueType>& a,
                               const Deque_iterator<ValueType>& b)
        {
            return a.cur != b.cur;
        }

        // -> разыменовывает указатель(возвращает ссылку на значение)
        reference operator*() const
        {
            return cur->value;
        }
        pointer operator->() const
        {
            return cur + sizeof(ValueType);
        }

        // this - указатель на текущий объект
        //*this - ссылка на текущий объект
        Deque_iterator& operator++(){
            cur = cur->next;
            return *this;
        }

        Deque_iterator operator++(int){
            Deque_iterator<ValueType> temp(*this);
            operator++();
            return temp;
        }

        Deque_iterator& operator--()
        {
            cur = cur->previous;
            return *this;
        }
        Deque_iterator operator--(int)
        {
            Deque_iterator<ValueType> temp(*this);
            operator--();
            return temp;
        }
            // перемещает итератор вперёд0 на a элементов
        Deque_iterator operator+(const difference_type& a) const
        {
            return Deque_iterator<ValueType>(*this) += a;
        }
        Deque_iterator& operator+=(const difference_type& a)
        {
            cur = cur + a;
            return *this;
        }

        Deque_iterator operator-(const difference_type& a) const
        {
            return Deque_iterator<ValueType>(*this) -= a;
        }
        Deque_iterator& operator-=(const difference_type& a)
        {
            cur = cur - a;
            return *this;
        }
        //  возвращает количество элементов между двумя итераторами
        difference_type operator-(const Deque_iterator& a) const
        {
            difference_type total = cur - a.cur;
            return abs(total);
        }

        reference operator[](const difference_type& a)
        {
            return *(cur + a);
        }

        friend bool operator<(const Deque_iterator<ValueType>& a,
                              const Deque_iterator<ValueType>& b)
        {
            return a.cur < b.cur;
        }
        friend bool operator<=(const Deque_iterator<ValueType>& a,
                               const Deque_iterator<ValueType>& b)
        {
            return a.cur <= b.cur;
        }
        friend bool operator>(const Deque_iterator<ValueType>& a,
                              const Deque_iterator<ValueType>& b)
        {
            return a.cur > b.cur;
        }
        friend bool operator>=(const Deque_iterator<ValueType>& a,
                               const Deque_iterator<ValueType>& b)
        {
            return a.cur >= b.cur;
        }
    };

    template <typename ValueType>
    class Deque_const_iterator {
        // Shouldn't give non const references on value
    public:
        using iterator_category = std::random_access_iterator_tag;
        using value_type = ValueType;
        using difference_type = std::ptrdiff_t;
        using pointer = ValueType*;
        using reference = ValueType&;
        Node<value_type> *cur;
        Deque_const_iterator() = default;

        Deque_const_iterator(const Deque_const_iterator& other) noexcept{
            cur = other.cur;
        }

        Deque_const_iterator& operator=(const Deque_const_iterator&& a)
        {
            cur = std::move(a.cur);
            return *this;
        }

        Deque_const_iterator& operator=(const Deque_const_iterator& a)
        {
            cur = a.cur;
            return *this;
        }

        ~Deque_const_iterator() = default;

        friend void swap(Deque_const_iterator<ValueType>& a, Deque_const_iterator<ValueType>& b){
            Deque_const_iterator<ValueType> c = std::move(a);
            a = std::move(b);
            b = std::move(c);
        }

        friend bool operator==(const Deque_const_iterator<ValueType>& a,
                               const Deque_const_iterator<ValueType>& b)
        {
            return a.cur == b.cur;
        }
        friend bool operator!=(const Deque_const_iterator<ValueType>& a,
                               const Deque_const_iterator<ValueType>& b)
        {
            return a.cur != b.cur;
        }

        reference operator*() const
        {
            return cur->value;
        }
        pointer operator->() const
        {
            return cur + sizeof(ValueType);
        }

        Deque_const_iterator& operator++(){
            cur = cur->next;
            return *this;
        }

        Deque_const_iterator operator++(int){
            Deque_const_iterator<ValueType> temp(*this);
            operator++();
            return temp;
        }

        Deque_const_iterator& operator--()
        {
            cur = cur->previous;
            return *this;
        }
        Deque_const_iterator operator--(int)
        {
            Deque_iterator<ValueType> temp(*this);
            operator--();
            return temp;
        }

        Deque_const_iterator operator+(const difference_type& a) const
        {
            return Deque_iterator<ValueType>(*this) += a;
        }
        Deque_const_iterator& operator+=(const difference_type& a)
        {
            cur = cur + a;
            return *this;
        }

        Deque_const_iterator operator-(const difference_type& a) const
        {
            return Deque_const_iterator<ValueType>(*this) -= a;
        }
        Deque_const_iterator& operator-=(const difference_type& a)
        {
            cur = cur - a;
            return *this;
        }
        difference_type operator-(const Deque_const_iterator& a) const
        {
            difference_type total = cur - a.cur;
            return abs(total);
        }

        reference operator[](const difference_type& a)
        {
            return *(cur + a);
        }

        friend bool operator<(const Deque_const_iterator<ValueType>& a,
                              const Deque_const_iterator<ValueType>& b)
        {
            return a.cur < b.cur;
        }
        friend bool operator<=(const Deque_const_iterator<ValueType>& a,
                               const Deque_const_iterator<ValueType>& b)
        {
            return a.cur <= b.cur;
        }
        friend bool operator>(const Deque_const_iterator<ValueType>& a,
                              const Deque_const_iterator<ValueType>& b)
        {
            return a.cur > b.cur;
        }
        friend bool operator>=(const Deque_const_iterator<ValueType>& a,
                               const Deque_const_iterator<ValueType>& b)
        {
            return a.cur >= b.cur;
        }
    };

    template <class Iter>
    class Deque_reverse_iterator {
    public:
        using iterator_type = Iter;
        using iterator_category = typename std::iterator_traits<Iter>::iterator_category;
        using value_type = typename std::iterator_traits<Iter>::value_type;
        using difference_type = typename std::iterator_traits<Iter>::difference_type;
        using pointer = typename std::iterator_traits<Iter>::pointer;
        using reference = typename std::iterator_traits<Iter>::reference;

        iterator_type _it;
        value_type* cur;
        constexpr Deque_reverse_iterator()= default;

        constexpr explicit Deque_reverse_iterator(iterator_type x){
            _it = x;
        }

        template <class U>
        constexpr Deque_reverse_iterator(const Deque_reverse_iterator<U>& other){
            _it = other._it;
        }

        template <class U>
        Deque_reverse_iterator& operator=(const Deque_reverse_iterator<U>&& other){
            _it = std::move(other._it);
            return *this;
        }

        template <class U>
        Deque_reverse_iterator& operator=(const Deque_reverse_iterator<U>& other){
            _it = other._it;
            return *this;
        }

        iterator_type base() const {
            return _it;
        }

        reference operator*() const {
            return *base();
        }

        pointer operator->() const {
            return base();
        }

        reference operator[](difference_type n){
            return *(base() + n);
        }

        Deque_reverse_iterator& operator++() {
            --_it;
            return *this;
        }
        Deque_reverse_iterator operator++(int val) {
            Deque_reverse_iterator tmp(*this);
            operator++();
            return tmp;
        }

        Deque_reverse_iterator& operator--() {
            ++_it;
            return *this;
        }
        Deque_reverse_iterator operator--(int val) {
            Deque_reverse_iterator tmp(*this);
            operator--();
            return tmp;
        }

        Deque_reverse_iterator operator+(difference_type n) const {
            iterator_type it = _it - n;
            return Deque_reverse_iterator<iterator_type>(it);
        }
        Deque_reverse_iterator& operator+=(difference_type n) {
            _it -= n;
            return reverse_iterator(_it);
        }

        Deque_reverse_iterator operator-(difference_type n) const {
            iterator_type it = _it + n;
            return Deque_reverse_iterator<iterator_type>(it);
        }
        Deque_reverse_iterator& operator-=(difference_type n) {
            _it += n;
            return reverse_iterator(_it);
        }

        template <class Iterator1, class Iterator2>
        friend bool operator==(const Deque_reverse_iterator<Iterator1>& lhs,
                               const Deque_reverse_iterator<Iterator2>& rhs){
            return lhs.base() == rhs.base();
        }

        template <class Iterator1, class Iterator2>
        friend bool operator!=(const Deque_reverse_iterator<Iterator1>& lhs,
                               const Deque_reverse_iterator<Iterator2>& rhs){
            return lhs.base() != rhs.base();
        }

        template <class Iterator1, class Iterator2>
        friend bool operator>(const Deque_reverse_iterator<Iterator1>& lhs,
                              const Deque_reverse_iterator<Iterator2>& rhs){
            return lhs.base() > rhs.base();
        }

        template <class Iterator1, class Iterator2>
        friend bool operator<(const Deque_reverse_iterator<Iterator1>& lhs,
                              const Deque_reverse_iterator<Iterator2>& rhs){
            return lhs.base() < rhs.base();
        }

        template <class Iterator1, class Iterator2>
        friend bool operator<=(const Deque_reverse_iterator<Iterator1>& lhs,
                               const Deque_reverse_iterator<Iterator2>& rhs){
            lhs.base() <= rhs.base();
        }

        template <class Iterator1, class Iterator2>
        friend bool operator>=(const Deque_reverse_iterator<Iterator1>& lhs,
                               const Deque_reverse_iterator<Iterator2>& rhs){
            lhs.base() >= rhs.base();
        }

        template <class IterT>
        friend Deque_reverse_iterator<IterT> operator+(
                typename Deque_reverse_iterator<IterT>::difference_type n,
                const Deque_reverse_iterator<IterT>& it){
            return Deque_reverse_iterator<IterT>(it.base() - n);
        }

        template <class Iterator>
        friend auto operator-(const Deque_reverse_iterator<Iterator>& lhs,
                              const Deque_reverse_iterator<Iterator>& rhs){
            return Deque_reverse_iterator<Iterator>(lhs.base() - rhs.base());
        }

        // operator <=> will be handy

        // friend constexpr std::iter_rvalue_reference_t<Iter> iter_move( const
        // std::reverse_iterator& i ); // For extra points

        // template<std::indirectly_swappable<Iter> Iter2>
        // friend constexpr void iter_swap(const reverse_iterator& x, const
        // std::reverse_iterator<Iter2>& y); // For extra points
    };

    template <typename T, typename Allocator = Allocator<Node<T>> >
    class Deque {
    public:
        using value_type = T;
        using allocator_type = Allocator;
        using size_type = std::size_t;
        using difference_type = std::ptrdiff_t;
        using reference = value_type&;
        using const_reference = const value_type&;
        using pointer = typename std::allocator_traits<Allocator>::pointer;
        using const_pointer = typename std::allocator_traits<Allocator>::const_pointer;
        using iterator = Deque_iterator<value_type>;
        using const_iterator = Deque_const_iterator<value_type>;
        using reverse_iterator = Deque_reverse_iterator<iterator>;
        using const_reverse_iterator = Deque_reverse_iterator<const_iterator>;

        Allocator alloc;
        Node<value_type>* first = nullptr;
        Node<value_type>* last = nullptr;
        value_type _size = 0;


        /// @brief Default constructor. Constructs an empty container with a
        /// default-constructed allocator.
        Deque(){}

        /// @brief Constructs an empty container with the given allocator
        /// @param alloc allocator to use for all memory allocations of this container
        explicit Deque(const Allocator& alloc)
        {
            Node<value_type> *node = alloc.allocate(1);
            first = node;
            last = node;
        }

        /// @brief Constructs the container with count copies of elements with value
        /// and with the given allocator
        /// @param count the size of the container
        /// @param value the value to initialize elements of the container with
        /// @param alloc allocator to use for all memory allocations of this container
        Deque(size_type count, const T& value, const Allocator& alloc = Allocator())
        {
            for(size_type i = 0; i < count; i++){
                push_back(value);
            }
        }

        /// @brief Constructs the container with count default-inserted instances of
        /// T. No copies are made.
        /// @param count the size of the container
        /// @param alloc allocator to use for all memory allocations of this container
        explicit Deque(size_type count, const Allocator& alloc = Allocator())
        {
            for(value_type i = 0; i < count; i++){
                push_back(0);
            }
        }

        /// @brief Constructs the container with the contents of the range [first,
        /// last).
        /// @tparam InputIt Input Iterator
        /// @param first, last 	the range to copy the elements from
        /// @param alloc allocator to use for all memory allocations of this container
        template <class InputIt>
        Deque(InputIt first, InputIt last, const Allocator& alloc = Allocator()){
            for(InputIt i = first; i != last; i++){
                push_back(*i);
            }
        }

        /// @brief Copy constructor. Constructs the container with the copy of the
        /// contents of other.
        /// @param other another container to be used as source to initialize the
        /// elements of the container with
        // конструктор копирования
        Deque(const Deque& other)
        {
            for(value_type i = 0; i < other.size(); i++){
                Node<value_type> *node = other.get_allocator().allocate(1);
                node->value = other[i];
                if(empty()){
                    this->first = node;
                    this->last = node;
                }
                else{
                    last -> next = node;
                    last = node;
                }
            }
            _size = other.size();
        }

        /**
         * @brief Move constructor.
         *
         * Constructs the container with the contents of other using move semantics.
         * Allocator is obtained by move-construction from the allocator belonging to
         * other.
         *
         * @param other another container to be used as source to initialize the
         * elements of the container with
         */
        Deque(Deque&& other) noexcept
        {
            for(value_type i = 0; i < other.size();i++){
                Node<value_type> *node = other.get_allocator().allocate(1);
                node->value = other[i];
                if(empty()){
                    first = node;
                    last = node;
                }
                else{
                    node->previous = last;
                    last -> next = node;
                    last = node;
                    first->previous = last;
                }
            }
            _size = other.size();
        }

        /**
         * @brief Allocator-extended move constructor.
         * Using alloc as the allocator for the new container, moving the contents
         * from other; if alloc != other.get_allocator(), this results in an
         * element-wise move.
         *
         * @param other another container to be used as source to initialize the
         * elements of the container with
         * @param alloc allocator to use for all memory allocations of this container
         */
        Deque(Deque&& other, const Allocator& alloc)
        {
            this->alloc = alloc;
            for(size_type i = 0; i < other.size();i++){
                push_back(other[i]);
            }
        }

        /// @brief Constructs the container with the contents of the initializer list
        /// init.
        /// @param init initializer list to initialize the elements of the container
        /// with
        /// @param alloc allocator to use for all memory allocations of this container
        Deque(std::initializer_list<T> init, const Allocator& alloc = Allocator()){
            for(auto i: init){
                push_back(i);
            }
        }

        /// @brief Destructs the deque.
        ~Deque(){
            Node<value_type>* current = first;
            while(current != nullptr) {
                Node<value_type>* next = current->next;
                alloc.deallocate(current);
                current = next;
            }
            // функция для исключений
            if (std::uncaught_exception()) {
                // используется для вывода ошибок
                std::cerr << "~Deque() stack unwinding, not throwing\n";
            } else {
                std::cerr << "~Deque() normal case, throwing\n";
            }
        }

        /// @brief Copy assignment operator. Replaces the contents with a copy of the
        /// contents of other.
        /// @param other another container to use as data source
        /// @return *this
        Deque& operator=(const Deque& other){
            clear();
            for(size_type i = 0; i < other.size();i++){
                push_back(other[i]);
            }
        }

        /**
         * Move assignment operator.
         *
         * Replaces the contents with those of other using move semantics
         * (i.e. the data in other is moved from other into this container).
         * other is in a valid but unspecified state afterwards.
         *
         * @param other another container to use as data source
         * @return *this
         */
        Deque& operator=(Deque&& other){
            clear();
            for(size_type i = 0; i < other.size();i++){
                push_back(std::move(other[i]));
            }
        }

        /// @brief Replaces the contents with those identified by initializer list
        /// ilist.
        /// @param ilist
        /// @return this
                        // класс-обёртка(содержит указатель на 1ый элемент списка и колтчество эл-тов)
        Deque& operator=(std::initializer_list<T> ilist){
            clear();
            for(auto i: ilist){
                push_back(*i);  // значение элемента
            }
        }

        /// @brief Replaces the contents with count copies of value
        /// @param count
        /// @param value
        // заполняет всё значением value
        void assign(size_type count, const T& value){
            resize(count, value);
        }

        /// @brief Replaces the contents with copies of those in the range [first,
        /// last).
        /// @tparam InputIt
        /// @param first
        /// @param last
        template <class InputIt>
        void assign(InputIt first, InputIt last){
            clear();
            for(InputIt i = first; i != last; i++){
                push_back(*i);
            }
        }

        /// @brief Replaces the contents with the elements from the initializer list
        /// ilis
        /// @param ilist
        void assign(std::initializer_list<T> ilist){
            clear();
            for(auto i: ilist){
                push_back(i);
            }
        }

        /// @brief Returns the allocator associated with the container.
        /// @return The associated allocator.
        allocator_type get_allocator() const noexcept
        {
            return alloc;
        }

        /// ELEMENT ACCESS

        /// @brief Returns a reference to the element at specified location pos, with
        /// bounds checking. If pos is not within the range of the container, an
        /// exception of type std::out_of_range is thrown.
        /// @param pos position of the element to return
        /// @return Reference to the requested element.
        /// @throw std::out_of_range
        // возвращает ссылку на элемент
        reference at(size_type pos){
            if(pos+1 > _size) throw std::out_of_range("index out of range");
            else return operator[](pos);
        }

        /// @brief Returns a const reference to the element at specified location pos,
        /// with bounds checking. If pos is not within the range of the container, an
        /// exception of type std::out_of_range is thrown.
        /// @param pos position of the element to return
        /// @return Const Reference to the requested element.
        /// @throw std::out_of_range
        const_reference at(size_type pos) const{
            if(pos+1 > _size) throw std::out_of_range("index out of range");
            else return operator[](pos);
        }

        /// @brief Returns a reference to the element at specified location pos. No
        /// bounds checking is performed.
        /// @param pos position of the element to return
        /// @return Reference to the requested element.
        reference operator[](size_type pos){
            size_type it = 0;
            for(Node<value_type>* cur = first; cur != nullptr;cur = cur->next)
            {
                if(pos == it)
                {
                    return cur->value;
                }
                it++;
            }
        }

        /// @brief Returns a const reference to the element at specified location pos.
        /// No bounds checking is performed.
        /// @param pos position of the element to return
        /// @return Const Reference to the requested element.
        const_reference operator[](size_type pos) const{
            size_type it = 0;
            for(Node<value_type>* cur = first; cur != nullptr;cur = cur->next)
            {
                if(pos == it)
                {
                    return cur->value;
                }
                it++;
            }
        }

        /// @brief Returns a reference to the first element in the container.
        /// Calling front on an empty container is undefined.
        /// @return Reference to the first element
        reference front(){
            return first->value;
        }

        /// @brief Returns a const reference to the first element in the container.
        /// Calling front on an empty container is undefined.
        /// @return Const reference to the first element
        const_reference front() const{
            return first->value;
        }

        /// @brief Returns a reference to the last element in the container.
        /// Calling back on an empty container causes undefined behavior.
        /// @return Reference to the last element.
        reference back(){
            return last->value;
        }

        /// @brief Returns a const reference to the last element in the container.
        /// Calling back on an empty container causes undefined behavior.
        /// @return Const Reference to the last element.
        const_reference back() const{
            return last->value;
        }

        /// ITERATORS

        /// @brief Returns an iterator to the first element of the deque.
        /// If the deque is empty, the returned iterator will be equal to end().
        /// @return Iterator to the first element.
        iterator begin() noexcept{
            iterator a;
            a.cur = first;
            return a;
        }

        /// @brief Returns an iterator to the first element of the deque.
        /// If the deque is empty, the returned iterator will be equal to end().
        /// @return Iterator to the first element.
        const_iterator begin() const noexcept{
            const_iterator a;
            a.cur = first;
            return a;
        }

        /// @brief Same to begin()
        const_iterator cbegin() const noexcept{
            const_iterator a;
            a.cur = first;
            return a;
        }

        /// @brief Returns an iterator to the element following the last element of
        /// the deque. This element acts as a placeholder; attempting to access it
        /// results in undefined behavior.
        /// @return Iterator to the element following the last element.
        iterator end() noexcept{
            Deque_iterator<value_type> a;
            a.cur = last;
            return a;
        }

        /// @brief Returns an constant iterator to the element following the last
        /// element of the deque. This element acts as a placeholder; attempting to
        /// access it results in undefined behavior.
        /// @return Constant Iterator to the element following the last element.
        const_iterator end() const noexcept{
            Deque_iterator<value_type> a;
            a.cur = last;
            return a;
        }

        /// @brief Same to end()
        const_iterator cend() const noexcept{
            const_iterator a;
            a.cur = last;
            return a;
        }

        /// @brief Returns a reverse iterator to the first element of the reversed
        /// deque. It corresponds to the last element of the non-reversed deque. If
        /// the deque is empty, the returned iterator is equal to rend().
        /// @return Reverse iterator to the first element.
        reverse_iterator rbegin() noexcept{
            // получаем обратный итератор a
            Deque_reverse_iterator <Deque_iterator<value_type> > a(begin());
            return a;
        }

        /// @brief Returns a const reverse iterator to the first element of the
        /// reversed deque. It corresponds to the last element of the non-reversed
        /// deque. If the deque is empty, the returned iterator is equal to rend().
        /// @return Const Reverse iterator to the first element.
        const_reverse_iterator rbegin() const noexcept{
            Deque_reverse_iterator <Deque_iterator<value_type> > a(begin());
            return a;
        }

        /// @brief Same to rbegin()
        const_reverse_iterator crbegin() const noexcept{
            Deque_reverse_iterator <Deque_iterator<value_type> > a(begin());
            return a;
        }

        /// @brief Returns a reverse iterator to the element following the last
        /// element of the reversed deque. It corresponds to the element preceding the
        /// first element of the non-reversed deque. This element acts as a
        /// placeholder, attempting to access it results in undefined behavior.
        /// @return Reverse iterator to the element following the last element.
        reverse_iterator rend() noexcept{
            Deque_reverse_iterator <Deque_iterator<value_type> > a(end());
            return a;
        }

        /// @brief Returns a const reverse iterator to the element following the last
        /// element of the reversed deque. It corresponds to the element preceding the
        /// first element of the non-reversed deque. This element acts as a
        /// placeholder, attempting to access it results in undefined behavior.
        /// @return Const Reverse iterator to the element following the last element.
        const_reverse_iterator rend() const noexcept{
            Deque_reverse_iterator <Deque_iterator<value_type> > a(end());
            return a;
        }

        /// @brief Same to rend()
        const_reverse_iterator crend() const noexcept{
            Deque_reverse_iterator <Deque_iterator<value_type> > a(end());
            return a;
        }

        /// CAPACITY

        /// @brief Checks if the container has no elements
        /// @return true if the container is empty, false otherwise
        bool empty() const noexcept{
            return first == nullptr;
        }

        /// @brief Returns the number of elements in the container
        /// @return The number of elements in the container.
        size_type size() const noexcept{
            return _size;
        }

        /// @brief Returns the maximum number of elements the container is able to
        /// hold due to system or library implementation limitations
        /// @return Maximum number of elements.
        // максимально возможное кол-во элементов дека
        size_type max_size() const noexcept{
            return std::numeric_limits<size_t>::max();
        }

        /// MODIFIERS

        /// @brief Erases all elements from the container.
        /// nvalidates any references, pointers, or iterators referring to contained
        /// elements. Any past-the-end iterators are also invalidated.
        void clear() noexcept{
            while (size() != 0){
                pop_back();
            }
        }

        /// @brief Inserts value before pos.
        /// @param pos iterator before which the content will be inserted.
        /// @param value element value to insert
        /// @return Iterator pointing to the inserted value.
        iterator insert(const_iterator pos, const T& value){
            Node<value_type>* cur = alloc.allocate(1);
            cur->value = value;
            // устанавливаем указатели для нового элемента
            cur->next = pos.cur;
            cur->previous = pos.cur->previous;
            // устанавливаем указатель предыдущего элемента на новый
            pos.cur->previous->next = cur;
            // устанавливаем укзатель следующего элемента на новый
            pos.cur->previous = cur;
            _size++;
            iterator a;
            a.cur = cur;
            return a;
        }

        /// @brief Inserts value before pos.
        /// @param pos iterator before which the content will be inserted.
        /// @param value element value to insert
        /// @return Iterator pointing to the inserted value.
        iterator insert(const_iterator pos, T&& value){
            Node<value_type>* cur = alloc.allocate(1);
            cur->value = value;
            cur->next = pos.cur;
            cur->previous = pos.cur->previous;
            pos.cur->previous->next = cur;
            pos.cur->previous = cur;
            _size++;
            iterator a;
            a.cur = cur;
            return a;
        }

        /// @brief Inserts count copies of the value before pos.
        /// @param pos iterator before which the content will be inserted.
        /// @param count number of elements to insert
        /// @param value element value to insert
        /// @return Iterator pointing to the first element inserted, or pos if count
        /// == 0.
        iterator insert(const_iterator pos, size_type count, const T& value){
            Node<value_type>* cur_;
            for(size_type i = 0; i < count; i++){
                Node<value_type>* cur = alloc.allocate(1);
                cur->value = value;
                cur->next = pos.cur;
                cur->previous = pos.cur->previous;
                pos.cur->previous->next = cur;
                pos.cur->previous = cur;
                _size++;
                cur_ = cur;
            }
            iterator a;
            a.cur = cur_;
            return a;
        }

        /// @brief Inserts elements from range [first, last) before pos.
        /// @tparam InputIt Input Iterator
        /// @param pos iterator before which the content will be inserted.
        /// @param first,last the range of elements to insert, can't be iterators into
        /// container for which insert is called
        /// @return Iterator pointing to the first element inserted, or pos if first
        /// == last.
        template <class InputIt>
        iterator insert(const_iterator pos, InputIt first, InputIt last){
            Node<value_type>* cur_;
            for(InputIt i = first; i != last; i++){
                Node<value_type>* cur = alloc.allocate(1);
                cur->value = *i;
                cur->next = pos.cur;
                cur->previous = pos.cur->previous;
                pos.cur->previous->next = cur;
                pos.cur->previous = cur;
                _size++;
                cur_ = cur;
            }
            iterator a;
            a.cur = cur_;
            return a;
        }

        /// @brief Inserts elements from initializer list before pos.
        /// @param pos iterator before which the content will be inserted.
        /// @param ilist initializer list to insert the values from
        /// @return Iterator pointing to the first element inserted, or pos if ilist
        /// is empty.
        iterator insert(const_iterator pos, std::initializer_list<T> ilist){
            Node<value_type>* cur_;
            for(auto i: ilist){
                Node<value_type>* cur = alloc.allocate(1);
                cur->value = i;
                cur->next = pos.cur;
                cur->previous = pos.cur->previous;
                pos.cur->previous->next = cur;
                pos.cur->previous = cur;
                _size++;
                cur_ = cur;
            }
            iterator a;
            a.cur = cur_;
            return a;
        }

        /// @brief Inserts a new element into the container directly before pos.
        /// @param pos iterator before which the new element will be constructed
        /// @param ...args arguments to forward to the constructor of the element
        /// @return terator pointing to the emplaced element.
        template <class... Args>
        // можно передавать lvalue и rvalue объекты
        iterator emplace(const_iterator pos, Args&&... args){
            Node<value_type>* cur = alloc.allocate(1);
            cur->value = value_type(args...);
            cur->next = pos.cur;
            cur->previous = pos.cur->previous;
            pos.cur->previous->next = cur;
            pos.cur->previous = cur;
            _size++;
            iterator a;
            a.cur = cur;
            return a;
        }

        /// @brief Removes the element at pos.
        /// @param pos iterator to the element to remove
        /// @return Iterator following the last removed element.
        iterator erase(const_iterator pos){
            if(pos.cur == first){
                if(_size == 1){
                    clear();
                }
                else {
                    Node<value_type> *cur = first;
                    first = first->next;
                    first->previous = nullptr;
                    _size--;
                    alloc.deallocate(cur);
                }
                iterator a;
                a.cur = first;
                return a;
            }
            if(pos.cur == last){
                Node<value_type> *cur = last;
                last = last->previous;
                last->next = nullptr;
                iterator a;
                a.cur = last;
                _size--;
                return a;
            }
            Node<value_type> *cur = pos.cur->next;
            pos.cur->previous->next = pos.cur->next;
            pos.cur->next->previous = pos.cur->previous;
            alloc.deallocate(pos.cur);
            _size--;
            iterator a;
            a.cur = cur;
            return a;
        }

        /// @brief Removes the elements in the range [first, last).
        /// @param first,last range of elements to remove
        /// @return Iterator following the last removed element.
        iterator erase(const_iterator first, const_iterator last){
            if(first == cbegin() && last == cend()){
                clear();
                iterator a;
                a.cur = this->first;
                return a;
            }
            if(first == cbegin()) {
                this->first = last.cur;
                this->first->previous = nullptr;
            }
            const_iterator cur = first;
            while (cur.cur != last.cur){
                erase(cur);
                cur++;
                _size--;
            }
            iterator a;
            a.cur = last.cur;
            return a;
        }

        /// @brief Appends the given element value to the end of the container.
        /// The new element is initialized as a copy of value.
        /// @param value the value of the element to append
        void push_back(const T& value){
            Node<value_type> *node = get_allocator().allocate(1);
            node->value = value;
            node->next = nullptr;
            node->previous = last;
            if(empty()){
                first = node;
                last = node;
            }
            else{
                last -> next = node;
                last = node;
            }
            _size++;
        }

        void push_back(T&& value){
            Node<value_type> *node = get_allocator().allocate(1);
            node->value = std::move(value);
            node->next = nullptr;
            node->previous = last;
            if(empty()){
                first = node;
                last = node;
            }
            else{
                last -> next = node;
                last = node;
            }
            _size++;
        }

        /// @brief Appends a new element to the end of the container.
        /// @param ...args arguments to forward to the constructor of the element
        /// @return A reference to the inserted element.
        template <class... Args>
        reference emplace_back(Args&&... args){
            pointer ref;
            for (auto v : {args...}) {
                ref = emplace_back(v);
            }
            reference result = *ref;
            return result;
        }

        /// @brief Removes the last element of the container.
        void pop_back(){
            if(last == first){
                alloc.deallocate(first);
                last = nullptr;
                first = nullptr;
                _size--;
                return;
            }
            Node<value_type> *del = last;
            last = last->previous;
            last->next = nullptr;
            get_allocator().deallocate(del);
            _size--;
        }

        /// @brief Prepends the given element value to the beginning of the container.
        /// @param value the value of the element to prepend
        void push_front(const T& value){
            Node<value_type>* node = get_allocator().allocate(1);
            node -> value = value;
            node->next = first;
            node->previous = nullptr;
            if (empty())
            {
                first = node;
                last = node;
                return;
            }
            first->previous = node;
            first = node;
            _size++;
        }

        /// @brief Prepends the given element value to the beginning of the container.
        /// @param value moved value of the element to prepend
        void push_front(T&& value){
            Node<value_type>* node = get_allocator().allocate(1);
            node -> value = std::move(value);
            node->next = first;
            node->previous = nullptr;
            if (empty())
            {
                first = node;
                last = node;
                return;
            }
            first->previous = node;
            first = node;
            _size++;
        }

        /// @brief Inserts a new element to the beginning of the container.
        /// @param ...args arguments to forward to the constructor of the element
        /// @return A reference to the inserted element.
        template <class... Args>
        reference emplace_front(Args&&... args){
            pointer ref;
            for (auto v : {args...}) {
                ref = emplace_front(v);
            }
            reference result = *ref;
            return result;
        }

        /// @brief Removes the first element of the container.
        void pop_front(){
            if(last == first){
                alloc.deallocate(first);
                last = nullptr;
                first = nullptr;
                _size--;
                return;
            }
            Node<value_type> *del = first;
            first = first->next;
            first->previous = nullptr;
            get_allocator().deallocate(del);
            _size--;
        }

        /// @brief Resizes the container to contain count elements.
        /// If the current size is greater than count, the container is reduced to its
        /// first count elements. If the current size is less than count, additional
        /// default-inserted elements are appended
        /// @param count new size of the container
        void resize(size_type count){
            while (_size > count){
                pop_back();
            }
            while (count > _size){
                push_back(value_type());
            }
        }

        /// @brief Resizes the container to contain count elements.
        /// If the current size is greater than count, the container is reduced to its
        /// first count elements. If the current size is less than count, additional
        /// copies of value are appended.
        /// @param count new size of the container
        /// @param value the value to initialize the new elements with
        void resize(size_type count, const value_type& value){
            while (_size > count){
                pop_back();
            }
            while (count > _size){
                push_back(value);
            }
        }

        /// @brief Exchanges the contents of the container with those of other.
        /// Does not invoke any move, copy, or swap operations on individual elements.
        /// All iterators and references remain valid. The past-the-end iterator is
        /// invalidated.
        /// @param other container to exchange the contents with
        // менеяем только указатели на начало и конец
        void swap(Deque& other){
            Node<value_type>* f = first;
            Node<value_type>* l = last;
            first = other.first;
            last = other.last;
            other.first = f;
            other.last = l;
        }

        /// COMPARISIONS

        /// @brief Checks if the contents of lhs and rhs are equal
        /// @param lhs,rhs deques whose contents to compare
        template <class U, class Alloc>
        friend bool operator==(const Deque<U, Alloc>& lhs,
                               const Deque<U, Alloc>& rhs){
            if(lhs.size() != rhs.size()) return false;
            for(size_type i = 0; i < lhs.size(); i++){
                if(rhs[i] != lhs[i]) return false;
            }
            return true;
        }

        /// @brief Checks if the contents of lhs and rhs are not equal
        /// @param lhs,rhs deques whose contents to compare
        template <class U, class Alloc>
        friend bool operator!=(const Deque<U, Alloc>& lhs, const Deque<U, Alloc>& rhs){
            if(lhs.size() != rhs.size()) return true;
            for(size_type i = 0; i < lhs.size(); i++){
                if(rhs[i] != lhs[i]) return true;
            }
            return false;
        }

        /// @brief Compares the contents of lhs and rhs lexicographically.
        /// @param lhs,rhs deques whose contents to compare
        template <class U, class Alloc>
        friend bool operator>(const Deque<U, Alloc>& lhs, const Deque<U, Alloc>& rhs){
            if(lhs.size() > rhs.size()) return true;
            else if(lhs.size() < rhs.size()) return false;
            for(size_type i = 0; i < lhs.size(); i++){
                if(rhs[i] == lhs[i]) continue;
                if(lhs[i] > rhs[i]) return true;
                return false;
            }
            return false;
        }

        /// @brief Compares the contents of lhs and rhs lexicographically.
        /// @param lhs,rhs deques whose contents to compare
        template <class U, class Alloc>
        friend bool operator<(const Deque<U, Alloc>& lhs, const Deque<U, Alloc>& rhs){
            if(lhs.size() < rhs.size()) return true;
            else if(lhs.size() < rhs.size()) return false;
            for(size_type i = 0; i < lhs.size(); i++){
                if(rhs[i] == lhs[i]) continue;
                if(lhs[i] < rhs[i]) return true;
                return false;
            }
            return false;
        }

        /// @brief Compares the contents of lhs and rhs lexicographically.
        /// @param lhs,rhs deques whose contents to compare
        template <class U, class Alloc>
        friend bool operator>=(const Deque<U, Alloc>& lhs,
                               const Deque<U, Alloc>& rhs){
            if(lhs.size() > rhs.size()) return true;
            else if(lhs.size() < rhs.size()) return false;
            for(size_type i = 0; i < lhs.size(); i++){
                if(rhs[i] == lhs[i]) continue;
                if(lhs[i] > rhs[i]) return true;
                return false;
            }
            return true;
        }

        /// @brief Compares the contents of lhs and rhs lexicographically.
        /// @param lhs,rhs deques whose contents to compare
        template <class U, class Alloc>
        friend bool operator<=(const Deque<U, Alloc>& lhs,
                               const Deque<U, Alloc>& rhs){
            if(lhs.size() < rhs.size()) return true;
            else if(lhs.size() < rhs.size()) return false;
            for(size_type i = 0; i < lhs.size(); i++){
                if(rhs[i] == lhs[i]) continue;
                if(lhs[i] < rhs[i]) return true;
                return false;
            }
            return true;
        }

        const_iterator get_iter(value_type val) const{
            for (Node<value_type>* cur = first; cur != nullptr; cur = cur->next)
            {
                if (cur->value == val)
                {
                    const_iterator a;
                    a.cur = cur;
                    return a;
                }
            }
            throw std::out_of_range("Element not found");
        }

        // operator <=> will be handy
    };

/// NON-MEMBER FUNCTIONS

/// @brief  Swaps the contents of lhs and rhs.
/// @param lhs,rhs containers whose contents to swap
    template <class T, class Alloc>
    void swap(Deque<T, Alloc>& lhs, Deque<T, Alloc>& rhs){
        Node<T> *first = lhs.first;
        Node<T> *last = lhs.last;
        lhs.first = rhs.first;
        lhs.last = rhs.last;
        rhs.first = first;
        rhs.last = last;
    }

/// @brief Erases all elements that compare equal to value from the container.
/// @param c container from which to erase
/// @param value value to be removed
/// @return The number of erased elements.
    template <class T, class Alloc>
    typename Deque<T, Alloc>::size_type erase(Deque<T, Alloc>& c, const T& value){
        for(size_t i = 0; i < c.size();i++){
            if(c[i] == value){
                c.erase(c.get_iter(value));
                return i;
            }
        }
    }

/// @brief Erases all elements that compare equal to value from the container.era
/// @param c container from which to erase
/// @param pred unary predicate which returns true if the element should be
/// erased.
/// @return The number of erased elements.
    template <class T, class Alloc, class Pred>
    typename Deque<T, Alloc>::size_type erase_if(Deque<T, Alloc>& c, Pred pred){
        for(size_t i = 0; i < c.size();i++){
            if(pred[i] == true) {
                c.erase(pred[i]);
                return i;
            }
        }
    }
}  // namespace fefu_laboratory_two