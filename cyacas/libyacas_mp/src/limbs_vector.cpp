#include "yacas/mp/limbs_vector.hpp"

#include <algorithm>
#include <cassert>

namespace yacas::mp {
    LimbsVector::LimbsVector() noexcept :
        m_data(std::array<Limb, 2>{}), m_size(0), m_capacity(2)
    {
    }

    LimbsVector::LimbsVector(const std::initializer_list<Limb>& limbs) :
        LimbsVector()
    {
        for (const auto limb : limbs) {
            push_back(limb);
        }
    }

    LimbsVector::LimbsVector(std::size_t n, Limb value) :
        m_size(n), m_capacity(n)
    {
        if (n <= 2) {
            m_data = std::array<Limb, 2>{};
        } else {
            m_data = std::make_unique<Limb[]>(n);
        }
        std::fill_n(begin(), n, value);
    }

    LimbsVector::LimbsVector(const LimbsVector& limbs) :
        m_size(limbs.size()), m_capacity(limbs.size())
    {
        if (limbs.size() <= 2) {
            m_data = std::get<0>(limbs.m_data);
        } else {
            m_data = std::make_unique<Limb[]>(limbs.size());
            std::copy(limbs.cbegin(), limbs.cend(), begin());
        }
    }

    LimbsVector::LimbsVector(LimbsVector&& limbs) noexcept :
        m_data(std::move(limbs.m_data)),
        m_size(limbs.m_size),
        m_capacity(limbs.m_capacity)
    {
        limbs.m_size = 0;
        limbs.m_capacity = 2;
    }

    std::strong_ordering
    LimbsVector::operator<=>(const LimbsVector& that) const noexcept
    {
        if (m_size != that.m_size)
            return m_size <=> that.m_size;

        for (std::size_t i = 0; i < m_size; ++i) {
            if (auto cmp = (*this)[i] <=> that[i]; cmp != 0)
                return cmp;
        }

        return std::strong_ordering::equal;
    }

    bool LimbsVector::operator==(const LimbsVector& that) const noexcept
    {
        return *this <=> that == 0;
    }

    bool LimbsVector::operator!=(const LimbsVector& that) const noexcept
    {
        return *this <=> that != 0;
    }

    LimbsVector& LimbsVector::operator=(const LimbsVector& other)
    {
        LimbsVector tmp(other);
        swap(tmp);
        return *this;
    }

    LimbsVector& LimbsVector::operator=(LimbsVector&& other) noexcept
    {
        swap(other);
        return *this;
    }

    void LimbsVector::swap(LimbsVector& other) noexcept
    {
        using std::swap;
        swap(m_data, other.m_data);
        swap(m_size, other.m_size);
        swap(m_capacity, other.m_capacity);
    }

    void LimbsVector::insert(iterator b, std::size_t n, Limb value)
    {
        const std::size_t index = b - begin();
        if (m_size + n > m_capacity)
            _grow(m_size + n);
        m_size += n;

        // FIXME: use std::move
        for (std::size_t i = m_size - n; i-- != index;)
            (*this)[i + n] = (*this)[i];

        std::fill_n(begin() + index, n, value);
    }

    void LimbsVector::erase(iterator b, iterator e)
    {
        assert(b >= begin() && e <= end() && b <= e);

        const std::size_t n = e - b;
        std::move(e, end(), b);
        m_size -= n;

        if (m_size <= 2) {
            if (m_data.index() == 1) {
                std::array<Limb, 2> new_data;
                std::copy_n(
                    std::get<1>(m_data).get(), m_size, new_data.begin());
                m_data = new_data;
                m_capacity = 2;
            }
        } else if (m_size < m_capacity / 2) {
            std::unique_ptr<Limb[]> new_data =
                std::make_unique<Limb[]>(m_capacity / 2);
            std::copy_n(std::get<1>(m_data).get(), m_size, new_data.get());
            m_data = std::move(new_data);
            m_capacity /= 2;
        }
    }

    LimbsVector::reverse_iterator LimbsVector::rbegin()
    {
        return std::make_reverse_iterator(end());
    }
    LimbsVector::reverse_iterator LimbsVector::rend()
    {
        return std::make_reverse_iterator(begin());
    }

    LimbsVector::const_reverse_iterator LimbsVector::crbegin() const
    {
        return std::make_reverse_iterator(cend());
    }
    LimbsVector::const_reverse_iterator LimbsVector::crend() const
    {
        return std::make_reverse_iterator(cbegin());
    }

    std::size_t LimbsVector::size() const noexcept
    {
        return m_size;
    }

    bool LimbsVector::empty() const noexcept
    {
        return m_size == 0;
    }

    LimbsVector::Limb* LimbsVector::data()
    {
        if (m_data.index() == 0)
            return std::get<0>(m_data).data();
        else
            return std::get<1>(m_data).get();
    }

    const LimbsVector::Limb* LimbsVector::data() const
    {
        if (m_data.index() == 0)
            return std::get<0>(m_data).data();
        else
            return std::get<1>(m_data).get();
    }

    LimbsVector::Limb& LimbsVector::operator[](std::size_t index)
    {
        assert(index < m_size);
        if (m_data.index() == 0)
            return std::get<0>(m_data)[index];
        else
            return std::get<1>(m_data)[index];
    }

    const LimbsVector::Limb& LimbsVector::operator[](std::size_t index) const
    {
        assert(index < m_size);
        if (m_data.index() == 0)
            return std::get<0>(m_data)[index];
        else
            return std::get<1>(m_data)[index];
    }

    LimbsVector::Limb& LimbsVector::front()
    {
        assert(m_size > 0);
        if (m_data.index() == 0)
            return std::get<0>(m_data)[0];
        else
            return std::get<1>(m_data)[0];
    }

    const LimbsVector::Limb& LimbsVector::front() const
    {
        assert(m_size > 0);
        if (m_data.index() == 0)
            return std::get<0>(m_data)[0];
        else
            return std::get<1>(m_data)[0];
    }

    LimbsVector::Limb& LimbsVector::back()
    {
        assert(m_size > 0);
        if (m_data.index() == 0)
            return std::get<0>(m_data)[m_size - 1];
        else
            return std::get<1>(m_data)[m_size - 1];
    }

    const LimbsVector::Limb& LimbsVector::back() const
    {
        assert(m_size > 0);
        if (m_data.index() == 0)
            return std::get<0>(m_data)[m_size - 1];
        else
            return std::get<1>(m_data)[m_size - 1];
    }

    void LimbsVector::clear()
    {
        m_data = std::array<Limb, 2>{};
        m_size = 0;
        m_capacity = 2;
    }

    void LimbsVector::push_back(Limb limb)
    {
        if (m_size == m_capacity)
            _grow(m_capacity * 2);

        if (m_data.index() == 0)
            std::get<0>(m_data)[m_size++] = limb;
        else
            std::get<1>(m_data)[m_size++] = limb;
    }

    void LimbsVector::pop_back()
    {
        assert(m_size > 0);

        m_size -= 1;

        if (m_data.index() == 1) {
            if (m_size <= 2) {
                std::array<Limb, 2> new_data;
                std::copy_n(
                    std::get<1>(m_data).get(), m_size, new_data.begin());
                m_data = new_data;
                m_capacity = 2;
            } else if (m_size < m_capacity / 2) {
                std::unique_ptr<Limb[]> new_data =
                    std::make_unique<Limb[]>(m_capacity / 2);
                std::copy_n(std::get<1>(m_data).get(), m_size, new_data.get());
                m_data = std::move(new_data);
                m_capacity /= 2;
            }
        }
    }

    void LimbsVector::resize(std::size_t n, Limb value)
    {
        if (n > m_size) {
            if (n > m_capacity)
                _grow(n);

            std::fill_n(end(), n - m_size, value);
            m_size = n;
        } else if (n < m_size) {
            m_size = n;
            if (m_data.index() == 1) {
                if (n <= 2) {
                    std::array<Limb, 2> new_data;
                    for (std::size_t i = 0; i < m_size; ++i)
                        new_data[i] = std::get<1>(m_data)[i];
                    m_data = new_data;
                    m_capacity = 2;
                } else if (n < m_capacity / 2) {
                    std::unique_ptr<Limb[]> new_data =
                        std::make_unique<Limb[]>(m_capacity / 2);
                    for (std::size_t i = 0; i < m_size; ++i)
                        new_data[i] = std::get<1>(m_data)[i];
                    m_data = std::move(new_data);
                    m_capacity /= 2;
                }
            }
        }
    }

    void LimbsVector::reserve(std::size_t new_capacity)
    {
        if (new_capacity > m_capacity)
            _grow(new_capacity);
    }

    void LimbsVector::_grow(std::size_t new_capacity)
    {
        assert(new_capacity > m_capacity);

        if (new_capacity > 2) {
            std::unique_ptr<Limb[]> new_data =
                std::make_unique<Limb[]>(new_capacity);
            std::copy_n(data(), m_size, new_data.get());
            m_data = std::move(new_data);
        }

        m_capacity = new_capacity;
    }

    LimbsVector::iterator LimbsVector::begin() noexcept
    {
        return data();
    }

    LimbsVector::iterator LimbsVector::end() noexcept
    {
        return data() + m_size;
    }

    LimbsVector::const_iterator LimbsVector::begin() const noexcept
    {
        return data();
    }

    LimbsVector::const_iterator LimbsVector::end() const noexcept
    {
        return data() + m_size;
    }

    LimbsVector::const_iterator LimbsVector::cbegin() const
    {
        return data();
    }

    LimbsVector::const_iterator LimbsVector::cend() const
    {
        return data() + m_size;
    }
}
