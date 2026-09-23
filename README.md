# URM
A language for writing register machines

### Installation Steps 
1. Update opam (Installation instructions for opam may be found [here](https://opam.ocaml.org/doc/Install.html))
```shell
opam update
```
2. Clone the repository 
```shell
git clone https://github.com/michaelleejl/register-machines.git
```
3. Navigate into the dir, create a [switch](https://ocaml.org/docs/opam-switch-introduction), and update the shell environment
```shell 
cd register-machines && opam switch create . --deps-only
eval $(opam env)
```
4. Build the project 
```shell 
dune build
```
5. Get the `urm` program on PATH (optional)
```shell 
opam install .
```

### Running a Program 
_If you chose to skip step 5, and do not have `urm` on PATH, 
then replace `urm` with `dune exec -- urm`._

The simplest way to run a program is like so (you can swap `programs/add.rm` for a program of your choice, various programs can be found in the `programs/` dir)
```shell
urm programs/add.rm
```

A `-v` flag (for _verbose_) prints out each step of the machine as a table 
```shell 
urm -v programs/add.rm
```

A `-b n` flag runs the program for `n` steps. Thus, intermediate 
states of nonterminating programs may be observed

```shell 
urm -n 5 programs/nonterminating.rm
```

### Statement on tool usage 
Pretty printing and error reporting are AI generated. 
The lexer and parser are AI generated, but human validated.
Everything else is my own work. 
