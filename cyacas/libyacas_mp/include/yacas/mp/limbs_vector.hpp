/*
 *
 * This file is part of yacas.
 * Yacas is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 2.1
 * of the License, or (at your option) any later version.
 *
 * Yacas is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with yacas.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#ifndef YACAS_MP_LIMBS_VECTOR_HPP
#define YACAS_MP_LIMBS_VECTOR_HPP

#include <array>
#include <compare>
#include <cstdint>
#include <memory>
#include <variant>

namespace yacas::mp {
    /// @brief A vector of limbs.
    /// @details This class is a simple vector of limbs. It is used to store the
    /// limbs of a multi-precision integer. It is implemented as a small buffer
    /// optimization. If the number of limbs is less than or equal to 2, the
    /// limbs are stored in a std::array. Otherwise, the limbs are stored in a
    /// std::unique_ptr. This allows the class to avoid dynamic memory
    /// allocation for small numbers of limbs. The class provides a simple
    /// interface for manipulating the limbs, including iterators, push_back,
    /// pop_back, and resizing. The class also provides a swap method for
    /// efficiently swapping the contents of two LimbsVector objects. The class
    /// is also comparable and hashable.
    class LimbsVector {
    public:
        typedef std::uint32_t Limb;
        typedef std::uint64_t Limb2;

        typedef Limb value_type;

        typedef std::uint32_t size_type;
        typedef std::int32_t difference_type;

        typedef value_type* iterator;
        typedef const value_type* const_iterator;
        typedef std::reverse_iterator<iterator> reverse_iterator;
        typedef std::reverse_iterator<const_iterator> const_reverse_iterator;

        /// @brief Default constructor.
        /// @details Constructs an empty LimbsVector.
        LimbsVector() noexcept;

        /// @brief Constructor from an initializer list.
        /// @details Constructs a LimbsVector from an initializer list of limbs.
        LimbsVector(const std::initializer_list<Limb>& limbs);

        /// @brief Constructor from a size and a value.
        /// @details Constructs a LimbsVector of a given size, with all limbs
        /// initialized to a given value.
        LimbsVector(std::size_t n, Limb value);

        /// @brief Copy constructor.
        /// @details Constructs a LimbsVector from another LimbsVector.
        explicit LimbsVector(const LimbsVector& limbs);

        /// @brief Move constructor.
        /// @details Constructs a LimbsVector by moving the contents of another
        /// LimbsVector.
        explicit LimbsVector(LimbsVector&& limbs) noexcept;

        /// @brief Constructor from a range.
        /// @details Constructs a LimbsVector from a range of limbs.
        template <typename Container>
        explicit LimbsVector(const Container& container)
        {
            for (const auto& limb : container)
                push_back(limb);
        }

        /// @brief Destructor.
        ~LimbsVector() = default;

        /// @brief Comparison operator.
        /// @details Compares two LimbsVector objects for ordering. The
        /// comparison is lexicographical, with the most significant limb
        /// compared first.
        std::strong_ordering
        operator<=>(const LimbsVector& that) const noexcept;

        /// @brief Equality comparison.
        /// @details Compares two LimbsVector objects for equality.
        bool operator==(const LimbsVector& that) const noexcept;

        /// @brief Inequality comparison.
        /// @details Compares two LimbsVector objects for inequality.
        bool operator!=(const LimbsVector& that) const noexcept;

        /// @brief Assignment operator.
        /// @details Assigns the contents of another LimbsVector to this one.
        LimbsVector& operator=(const LimbsVector& other);

        /// @brief Move assignment operator.
        /// @details Moves the contents of another LimbsVector to this one.
        LimbsVector& operator=(LimbsVector&& other) noexcept;

        template <typename InputIterator>
        void assign(InputIterator b, InputIterator e)
        {
            clear();
            for (auto it = b; it != e; ++it)
                push_back(*it);
        }

        /// @brief Swaps the contents of two LimbsVector objects.
        /// @details Swaps the contents of two LimbsVector objects in constant
        /// time.
        void swap(LimbsVector& other) noexcept;

        /// @brief Inserts a value at a given position.
        /// @details Inserts n copies of a value at a given position in the
        void insert(iterator b, std::size_t n, Limb value);

        /// @brief Erases a range of values.
        /// @details Erases a range of values from the LimbsVector.
        void erase(iterator b, iterator e);

        /// @brief Returns an iterator to the beginning of the LimbsVector.
        iterator begin() noexcept;
        /// @brief Returns an iterator to the end of the LimbsVector.
        iterator end() noexcept;

        /// @brief Returns a const iterator to the beginning of the LimbsVector.
        const_iterator begin() const noexcept;
        /// @brief Returns a const iterator to the end of the LimbsVector.
        const_iterator end() const noexcept;

        /// @brief Returns a const iterator to the beginning of the LimbsVector.
        const_iterator cbegin() const;
        /// @brief Returns a const iterator to the end of the LimbsVector.
        const_iterator cend() const;

        /// @brief Returns a reverse iterator to the beginning of the
        /// LimbsVector.
        reverse_iterator rbegin();
        /// @brief Returns a reverse iterator to the end of the LimbsVector.
        reverse_iterator rend();

        /// @brief Returns a const reverse iterator to the beginning of the
        /// LimbsVector.
        const_reverse_iterator crbegin() const;
        /// @brief Returns a const reverse iterator to the end of the
        /// LimbsVector.
        const_reverse_iterator crend() const;

        /// @brief Clears the LimbsVector.
        /// @details Removes all limbs from the LimbsVector.
        void clear();

        /// @brief Returns the number of limbs in the LimbsVector.
        std::size_t size() const noexcept;

        /// @brief Returns true if the vector is empty, false otherwise.
        bool empty() const noexcept;

        /// @brief Appends a limb to the end of the LimbsVector.
        void push_back(Limb limb);

        /// @brief Removes the last limb from the LimbsVector.
        void pop_back();

        /// @brief Resizes the LimbsVector.
        /// @details Resizes the LimbsVector to a given size. If the new size is
        /// greater than the current size, the new limbs are initialized to a
        /// given value. If the new size is less than the current size, the
        /// extra limbs are removed.
        void resize(std::size_t n, Limb value = 0);

        /// @brief Reserves space for a given number of limbs.
        /// @details Reserves space for a given number of limbs. If the new
        /// capacity is greater than the current capacity, the capacity is
        /// increased. If the new capacity is less than the current capacity,
        /// the capacity is decreased.
        void reserve(std::size_t new_capacity);

        /// @brief Returns a pointer to the underlying data.
        Limb* data();
        /// @brief Returns a pointer to the underlying data.
        const Limb* data() const;

        /// @brief Returns a reference to the limb at a given index.
        /// @details Returns a reference to the limb at a given index. The index
        /// must be less than the size of the LimbsVector.
        Limb& operator[](std::size_t index);
        const Limb& operator[](std::size_t index) const;

        /// @brief Returns a reference to the first limb.
        /// @details Returns a reference to the first limb in the LimbsVector.
        /// The LimbsVector must not be empty.
        Limb& front();
        /// @brief Returns a const reference to the first limb.
        /// @details Returns a const reference to the first limb in the
        /// LimbsVector. The LimbsVector must not be empty.
        const Limb& front() const;

        /// @brief Returns a reference to the last limb.
        /// @details Returns a reference to the last limb in the LimbsVector.
        /// The LimbsVector must not be empty.
        Limb& back();
        /// @brief Returns a const reference to the last limb.
        /// @details Returns a const reference to the last limb in the
        /// LimbsVector. The LimbsVector must not be empty.
        const Limb& back() const;

    private:
        std::variant<std::array<Limb, 2>, std::unique_ptr<Limb[]>> m_data;
        std::size_t m_size;
        std::size_t m_capacity;

        void _grow(std::size_t new_capacity);
    };
}

#endif