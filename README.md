# cmd2sankey

## Terminal %>% Graphical User Interface

Historically graphical user interface (GUI) stems from terminal. 

I emulate terminal enabling users to build sankey diagram with short commands. That’s why this app is called cmd2sankey.

## S3 is ideal for programming terminal

S# is the earliset one of R's OO paradigms. I implement all terminal commands and undo-commands to them with generic methods.

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

**First off all; before pressing "Enter" user should wait for a little bit. If given too little time, the "terminal" may lose you command's last argument/arguments**

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
