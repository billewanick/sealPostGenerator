# Seal Blog Post Generator

> Generate any number of blog posts about seals!

## Why Seals?

It's an in-joke.

## What's all this code?

- generate/
  - generateSealPosts.hs
    - A Haskell script that checks the website/posts folder and creates a blog post for every day from 1998 until the current date. There you will find the adjective lists if you think of more words to describe seals.
- dist/
  - Place for the Haskell build artifacts to go.

## These seals need more adjectives

In [config.dhall](https://git.ewanick.com/bill/sealPostGenerator/src/branch/main/config.dhall) you'll find two lists of adjectives. For the repo and add your own! Editing dhall config files does not require recompilation of the Haskell executable!

## Recent work

- Should running the generate function replace existing posts, or not? Probably not.
  - But need to split out the archive build vs the incremental post creation.
