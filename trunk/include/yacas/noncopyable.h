#ifndef YACAS_NONCOPYABLE_H
#define YACAS_NONCOPYABLE_H

class NonCopyable {
protected:
#ifndef YACAS_NO_CONSTEXPR
    constexpr
#endif
    NonCopyable() = default;
    ~NonCopyable() = default;
private:
    NonCopyable(const NonCopyable&) = delete;
    NonCopyable& operator=(const NonCopyable&) = delete;
};

#endif
