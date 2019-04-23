> This project has two homes.
> It is ok to work in github, still, for a better decentralized web
> please consider contributing (issues, PR, etc...) throught:
>
> https://gitlab.esy.fun/yogsototh/lish

---


lish
==========

[![Build Status](https://travis-ci.org/yogsototh/lish.svg?branch=master)](https://travis-ci.org/yogsototh/lish)

This project is an experimental LISP flavoured Shell

## Build

Install [`stack`](http://haskellstack.org)

And then

~~~
git clone https://github.com/yogsototh/lish.git
cd lish
stack setup && stack build
stack exec -- lish-exe
~~~

## To note

This Haskell project use the stack template `tasty-travis`.

Please read file `tutorial.md` for first steps in using the template.
