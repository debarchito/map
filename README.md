## 1. Development

The preferred way to develop `map` is to make use of [nix](https://nixos.org)
and [direnv](https://direnv.net).

```fish
direnv allow
```

Now that the development shell is active, `map` can either be built using `dune`
or `nix`.

```fish
dune build
# or
nix build
# or
nix run
```

> NOTE: We do not make use of [opam](https://opam.ocaml.org) for managing
> dependencies. Rather, add the required dependencies in
> [dune-project](/dune-project) and run `dune build`. This will fail but will
> prepare [map.opam](/map.opam) for the next step. To install the dependencies,
> run `direnv reload` and they'll be made available for subsequent `dune build`
> and `nix build`/`nix run` invocations.

## 2. Licensing

`map` is licensed under [GNU GPL v3](/LICENSE).
