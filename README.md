# cmd2sankey

It's a demo of R S3 paradigm for pre-GUI terminal.

The app is deployed on shinyapps.io: https://leonid-tkach.shinyapps.io/cmd2sankey/

## Terminal vs Graphical User Interface

Historically graphical user interface (GUI) stems from terminal. 

I emulate terminal enabling users to build sankey diagram with short commands. That’s why this app is called cmd2sankey.

I am going to develop optimal GUI later based on the terminal experience. **I believe that optimal set of terminal commands is able to translate into optimal GUI.**

## S3 is ideal for programming terminal

S3 is the earliest of R's OO paradigms. I implement all terminal commands and undo-commands to them with generic methods:

```{r}
run_command <- function(cmnd) { # S3
  UseMethod("run_command")
}
  
run_command.nu <- function(cmnd) {
  # add new user
}

run_command.cu <- function(cmnd) {
  # choose user = login
}

# and so on

undo_command <- function(cmnd) { # S3
  UseMethod("undo_command")
}

undo_command.nu <- function(cmnd) {
  # undo nu - delete user
}

# there is no undo-command for cu

# undo-commands are launched with command "u" in terminal (see maual below)
```


## Manual

**Important: before pressing "Enter" a user should wait for a little bit.** If given too little time, the "terminal" may lose your command's last argument/arguments.


Examples of commands:

```
> cu 1 leo
> nn node4
> nl 4 9 link3
> nl 9 5 link4
```

Here is a snippet of code as a manual:

```{r}
commands <- reactiveVal(tribble( # terminal commands & args
  ~cmd, ~args,
  'nu', c('nm'), # new user
  'cu', c('i1', '_pw'), # choose user (i1 is nu-command's cmd_i in the dataset
                        # arg with a name starting with '_' does not have its col in dataset
  'nc', c('nm'), # new country
  'ns', c('nm'), # new state
  'uc', c('i1'), # user's country; i1 is county indicator in dataset, one by one
  'u', c(), # undo last command of the current user
  'rn', c('cmd_i', '_nnm'), # new name
                            # arg with a name starting with '_' does not have its col in dataset
  'nn', c('nm'), # new node named nm
  'nl', c('i1', 'i2', 'nm') # new link named nm from node cmd_i==i1 to node cmd_i==i2
))
```
