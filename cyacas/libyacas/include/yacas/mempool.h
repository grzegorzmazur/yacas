#ifndef YACAS_MEMPOOL_H
#define YACAS_MEMPOOL_H

#include "noncopyable.h"

#include <cstddef>
#include <cstdint>

class MemPool: NonCopyable {
public:
    explicit MemPool(unsigned block_size, unsigned no_blocks);
    ~MemPool() noexcept;

    void* alloc();
    void free(void *p) noexcept;

private:
    unsigned _block_size;
    unsigned _no_blocks;

    unsigned _no_free_blocks;
    unsigned _no_initialized_blocks;

    std::uint8_t* _pool;
    
    std::uint8_t* _next_free_block;
    
    MemPool* _next_pool;
};

template <typename T>
class FastAlloc {
public:
    static void* operator new(std::size_t size) { return _pool.alloc(); }
    static void operator delete(void* p) { _pool.free(p); }
private:
    static MemPool _pool;
};

template <typename T>
MemPool FastAlloc<T>::_pool(sizeof (T), 32768);

#endif
