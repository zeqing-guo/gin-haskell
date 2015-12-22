# gin-haskell

Do you want to use your github issues as a static website like [this](https://github.com/lifesinger/blog/issues)? Gin can help you!

Gin is a project written in haskell which can parser your posts and transform them to github issues automatically.

## Features

:white_check_mark: Support Markdown grammar that [Github supports](https://help.github.com/articles/github-flavored-markdown/) (because I just upload your post with a few midifications :stuck_out_tongue_winking_eye:)

:white_check_mark: Support creating and modifying your issues posts locally

:white_check_mark: Support LaTeX grammar

:white_check_mark: Support post tags

:white_check_mark: Support some extension grammars

There is an [example](https://github.com/zeqing-guo/gin-haskell/issues/2).

## Installation

Please install [cabal](https://www.haskell.org/cabal/download.html) or [haskell-platform](https://www.haskell.org/platform/) first.

```
git clone https://github.com/zeqing-guo/gin-haskell.git & cd gin-haskell
cabal sandbox init --only-dependencies
cabal install
```

## Usage

```
➜  gin -h
Usage: gin [-inphv] [file]
  -i DIR   --init=DIR  Create a blog including a config file and an example post
  -n FILE  --new=FILE  Create a post
  -p       --publish   Publish new and modified posts on Github
  -h       --help      Print this help message
  -v       --version   Show gin's version

➜  gin -i blog
Copy data to blog

➜  cd blog 
➜  blog git:(master) ✗ gin -n "a new post"
Create post/2015-12-12 a new post.md

➜  echo "\n> hello world" >> post/2015-12-12\ a\ new\ post.md
➜  gin -p 
Upload a new post...
All posts have been updated
```

Note that you need to fill the _config.yml file to make gin work.

You can get a github token from [https://github.com/settings/tokens](https://github.com/settings/tokens), and `public_repo` is enough.

You can insert your copyright statement using `{{ copyright }}`.

## TODO List

1. Test on Windows
2. Improving performamce
3. Refactoring

BTW, the name **Gin** is from [Gintama (銀魂)](https://en.wikipedia.org/wiki/Gin_Tama) which is a funny comic. I hope you'll like it :smiling_imp:
