#pragma once

#include <atomic>
#include <exception>
#include <future>

class CancelledFuture final : public std::exception {
public:
    const char *what() const noexcept override { return "future was cancelled"; }
};

template <typename T> struct CancelableFutureState final {
    std::atomic<bool> cancelled{ false };
    std::promise<T> promise;

    void cancel() noexcept
    {
        try {
            promise.set_exception(std::make_exception_ptr(CancelledFuture{}));
        }
        catch (const std::future_error &) {
            // The terminal callback won the race with cancellation.
        }
    }
};

template <typename T> class CancelableFuture final {
public:
    explicit CancelableFuture(CancelableFutureState<T> *state) noexcept
        : state_(state)
        , future_(state->promise.get_future())
    {
    }

    CancelableFuture(CancelableFuture &&) = default;
    CancelableFuture &operator=(CancelableFuture &&) = default;
    CancelableFuture(const CancelableFuture &) = delete;
    CancelableFuture &operator=(const CancelableFuture &) = delete;

    void cancel() noexcept { state_->cancelled.store(true); }
    void wait() { future_.wait(); }
    T get() { return future_.get(); }

private:
    CancelableFutureState<T> *state_;
    std::future<T> future_;
};
