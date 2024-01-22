# BangBangGo

Browser extension that helps you easily navigate to search pages:
E.g. just type '!!yt cat video' to search YouTube, '!!gh some project' to search GitHub, '!dd foo' to search devdocs etc.

There's thousands (i.e. ~15K) of these 'bangs' to choose from. To see if you're favorite site is included: '!!bangs
name or domain'.

## Dev

### Setup

``` shell
$ yarn install
# TODO move dependencies in package.json dev-command to an alias
$ yarn dev
# start a watch-build for :ext
# unpack the ext-folder as extension in browser
# click on 'inspect views *service worker*'
# in cider: cider-connect-cljs, (shadow/repl :ext)
```

### Tests

``` shell
$ yarn ci:test:compile
$ yarn ci:test:run
```

## License

Copyright (c) 2024 Gert Goet, ThinkCreate. Distributed under the MIT license. See [LICENSE](./LICENSE).
