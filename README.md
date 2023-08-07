# Seal Blog Post Generator

> Generate any number of blog posts about seals!

## Why Seals?

It's an in-joke.

## What's all this code?

- [config.dhall](./config.dhall)
  - Typed non-deterministic config file for modifying the generator.
- [flake.nix](./flake.nix)
  - Entry point for all the nix ways of running and building the code.
- [generateSealPosts.hs](./generateSealPosts.hs)
  - A Haskell script that creates a blog post for every day from the configured date until the current day.

## These seals need more adjectives

In [config.dhall](./config.dhall) you'll find two lists of adjectives. For the repo and add your own! Editing dhall config files does not require recompilation of the Haskell executable!
