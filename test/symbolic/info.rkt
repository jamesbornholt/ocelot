#lang info

(define test-omit-paths (if (getenv "PLT_PKG_BUILD_SERVICE") 'all '()))