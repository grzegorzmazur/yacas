#include "yacas/mempool.h"

#include <algorithm>
#include <cassert>

MemPool::MemPool(unsigned block_size, unsigned no_blocks) :
    _block_size(std::max(static_cast<std::size_t>(block_size), sizeof(void*))),
    _no_blocks(no_blocks),
    _no_free_blocks(no_blocks),
    _no_initialized_blocks(0),
    _pool(new std::uint8_t[_block_size * _no_blocks]),
    _next_free_block(_pool),
    _next_pool(nullptr)
{
}

MemPool::~MemPool() noexcept
{
    assert(_no_free_blocks == _no_blocks);

    delete _next_pool;
    delete[] _pool;
}

void* MemPool::alloc()
{
    if (_no_free_blocks) {
        if (_no_initialized_blocks <= _no_blocks - _no_free_blocks) {
            std::uint8_t* p = _pool + _no_initialized_blocks * _block_size;
            *reinterpret_cast<std::uint8_t**>(p) = p + _block_size;
            _no_initialized_blocks += 1;
        }

        void* ret = _next_free_block;

        if (--_no_free_blocks)
            _next_free_block =
                *reinterpret_cast<std::uint8_t**>(_next_free_block);
        else
            _next_free_block = nullptr;

        return ret;
    }

    if (!_next_pool)
        _next_pool = new MemPool(_block_size, _no_blocks);

    return _next_pool->alloc();
}

void MemPool::free(void* p) noexcept
{
    if (p >= _pool && p < _pool + _block_size * _no_blocks) {
        if (_next_free_block) {
            *reinterpret_cast<std::uint8_t**>(p) = _next_free_block;
            _next_free_block = static_cast<std::uint8_t*>(p);
        } else {
            *reinterpret_cast<std::uint8_t**>(p) = _pool + _no_blocks;
            _next_free_block = static_cast<std::uint8_t*>(p);
        }
        _no_free_blocks += 1;
    } else {
        _next_pool->free(p);
    }
}
