## 1. Development

The preferred way to develop `map` is using [Nix](https://nixos.org) and
[direnv](https://direnv.net).

```fish
direnv allow
```

Once the development shell is active, `map` can either be built using `dune` or
`Nix`.

```fish
dune build
# or
nix build
# or
nix run
```

> NOTE: We do not use [opam](https://opam.ocaml.org). Add the required
> dependencies to [dune-project](/dune-project) and run `dune build`. This will
> fail but will prepare [map.opam](/map.opam) for the next step. To install the
> dependencies, run `direnv reload` and they'll be made available for subsequent
> `dune build` and `nix build`/`nix run` invocations.

## 2. Formatting

To format run:

```fish
# either
nix fmt
# or
nix run .#format
```

## 3. Licensing

`map` is licensed under [GNU GPL v3](/LICENSE).
