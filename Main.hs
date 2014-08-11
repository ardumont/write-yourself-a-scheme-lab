module Main where

import System.Environment
import Scheme.REPL ( runOne
                   , runRepl)

main :: IO ()
main =
  getArgs >>=
  \ args -> if null args then runRepl ":q" "lisp>>> " else runOne args

-- *Main> :main "(cons 2 3)"
-- (2 . 3)
-- *Main> :main
-- lisp>>> (cons 2 3)
-- (2 . 3)
-- lisp>>> (cons 2 '())
-- (2)
-- lisp>>> (cons 2 '(3))
-- (2 3)
-- lisp>>> :q
-- *Main> :browse Data.IORef

-- *Main> :main
-- lisp>>> (+ 1 1)
-- 2
-- lisp>>> (+ 1 10)
-- 11
-- lisp>>> (define (f x y) (+ x y))
-- (lambda ("x" "y") ...)
-- lisp>>> (f 1 2)
-- 3
-- lisp>>> (define x 10)
-- 10
-- lisp>>> (f 1 2)
-- 3
-- lisp>>> (f 10 2)
-- 12
-- lisp>>> (f 10 x)
-- 20
-- lisp>>> ((lambda (x y) (* x y)) 10 10)
-- 100
-- lisp>>> ((lambda (x y) (* x y)) 10 x)ef
-- 100
-- lisp>>> ((lambda (x y) (* x y)) 10 x)
-- 100
-- lisp>>> (define x 20)
-- 20
-- lisp>>> ((lambda (x y) (* x y)) 10 x)ef
-- 200
-- lisp>>> (set! x 200)
-- 200
-- lisp>>> ((lambda (x y) (* x y)) 10 x)
-- 2000
-- lisp>>> (define (f) "hello")
-- (lambda () ...)
-- lisp>>> f
-- (lambda () ...)
-- lisp>>> (f)
-- "hello"
-- lisp>>> (define (mute!) (set! x (+ x 20)))
-- (lambda () ...)
-- lisp>>> x
-- 200
-- lisp>>> (mute!)
-- 220
-- lisp>>> x
-- 220
-- lisp>>> (mute!)
-- 240
-- lisp>>> x
-- 240
-- lisp>>> :q
