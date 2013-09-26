# typescript-docs

A documentation tool for TypeScript Definition files

### Features

- Hyperlink generation
- Basic module support
- Support for simple jsdoc-style comments

### Installation

First install the [Haskell Platform](http://www.haskell.org/platform/).

Now install the `typescript-docs` package:

```
cabal install typescript-docs
```

Alternatively, clone and build locally:

```
git clone git@github.com:paf31/typescript-docs.git
cd typescript-docs
cabal install
```

### Usage

Generate declaration files as part of your build:

```
tsc -d ...
```

Include comments:

```
tsc -d -c ...
```

Run `typescript-docs`

```
typescript-docs *.d.ts
```

### Getting Help

```
typescript-docs --help
```

### Styling

You may wish to style the following classes using css:

- `keyword`
- `syntax`
- `identifier`
- `literal`
- `mono`
