#===============================================================================
# global variables

load('dataset.RData')

dataset <- reactiveVal(dataset_nr)

rm(dataset_nr)

# dataset <- reactiveVal(tibble(
#   u_i = numeric(), # current user who run the command
#   cmd_i = numeric(), # command index (user runs commands one by one)
#   cmd = character(), # command name
#   nm = character(),
#   i1 = numeric(),
#   i2 = numeric(),
#   dt = as_datetime(numeric(), tz = 'EST'),
#   m = character(),
#   d = numeric(),
#   t = character()
# ))

commands <- reactiveVal(tribble(
  ~cmd, ~args,
  'nu', c('nm'), # new user
  'cu', c('i1', '_pw'), # choose user (i1 is user's indicator in dataset, one by one
                        # arg with a name starting with '_' does not have its col in dataset
  'nc', c('nm'), # new country
  'ns', c('nm'), # new state
  'uc', c('i1') # user's country; i1 is county indicator in dataset, one by one
))

current_users <- reactiveVal(tibble(
  u_ind = numeric(), # index number of a known user (from dataset)
  u_gnm = character() # user's generated unique name
))

cur_cmd_ind <- reactiveVal(nrow(isolate(dataset())))

# global variables
#===============================================================================

function(input, output, session) {
  command <- reactiveVal('')
  log_str <- reactiveVal('')
  
#===============================================================================
# supporting current users()
  cu_gnm <- reactiveVal('') # current user's generated unique name
  cu_ind <- reactiveVal(0) # current user's indicator form current_users
  
  # browser()
  
  session_init <- FALSE
  
  session$onSessionEnded(function() {
    # browser()
    current_users(isolate({
      current_users() %>% filter(u_gnm != cu_gnm)
    }))
    
    # browser()
    dataset_nr <- isolate(dataset())
    save(dataset_nr, 
         file = 'dataset.RData',
         envir = environment())
  })
  
  if (!session_init) {
    # Seed username
    cu_gnm <- paste0("User", round(runif(1, 10000, 99999)))
    current_users(isolate(
      current_users() %>% 
        add_row(u_gnm = cu_gnm,
                u_ind = 0)
    ))
    session_init <- TRUE
  }
# supporting current users()
#===============================================================================
  
#===============================================================================
# supporting ui
  output$dataset <- renderReactable({
    reactable(dataset() %>% 
                arrange(desc(cmd_i)) %>% 
                select(-dt),
              defaultColDef = colDef(minWidth = 50))
  })
  
  output$current_users <- renderReactable({
    reactable(current_users(),
              defaultColDef = colDef(minWidth = 50))
  })
  
  output$log <- renderUI({
    HTML(log_str())
  })
  
  observeEvent(list(input$keys, input$run), {
    # browser()
    command(input$tmnl)
  })
# supporting ui
#===============================================================================

#===============================================================================
# supporting run_command()
  delete_last_row_in_dataset <- function() {
    dataset(isolate(
      head(dataset(), -1)
    ))
  }
  
  add_to_log_str <- function(str_to_add, cmd_wrng) {
    log_str(paste0(if_else(cmd_wrng == 'cmd', '> ', ''), 
                   str_to_add, '<br/>', 
                   log_str()))
  }
  
  observeEvent(command(), {
    # browser()
    if (command() == '') {
      return()
    }
    add_to_log_str(command(), 'cmd')
    isolate(updateTextInput(session, 'tmnl', value = ''))
    cmnd_vec <- str_extract_all(command(), '(\\w+)') %>% 
      unlist()
    # if user tries do anything before signing in
    # exception: adding the very first user
    if (cu_ind() == 0 && nrow(dataset()) > 0 && cmnd_vec[1] != 'cu') {
      add_to_log_str( 'Please sign in!', 'wrng')
      return()
    }
    cmnd <- structure(class = cmnd_vec[1],
                      list(args = cmnd_vec[-1]))
    # browser()
    args_num <- length(commands()$args[commands()$cmd == class(cmnd)] %>% unlist())
    if (length(cmnd$args) < args_num) {
      add_to_log_str(paste0('Command "', class(cmnd), '" has ',
                            args_num, ' args, but you have only provided ',
                            length(cmnd$args), ' args.'), 'wrng')
      return()
    }
    rc_res <- run_command(cmnd)
    if (rc_res$add_to_ds) { # cmnd is ok to add to dataset
      now_dt <- now(tzone = 'EST')
      dataset(isolate(
        dataset() %>% 
          add_row(u_i = cu_ind(),
                  cmd_i = nrow(dataset()) + 1,
                  cmd = cmnd_vec[1],
                  nm = rc_res$nm,
                  i1 = rc_res$i1,
                  i2 = rc_res$i2,
                  dt = now_dt,
                  m = month(now_dt, label = TRUE),
                  d = day(now_dt),
                  t = format(now_dt, format = "%H:%M:%S")
          )
      ))
    }
  })
  
  run_command <- function(cmnd) {
    UseMethod("run_command")
  }
  
  run_command.default <- function(cmnd) {
    add_to_log_str(paste0('Command "', class(cmnd), '" is unknown!'), 
                   'wrng')
  }
  
  run_command.nu <- function(cmnd) {
    # browser()
    users_num <- dataset() %>% filter(cmd == 'nu') %>% nrow()
    if ((users_num > 0 && cu_ind() == 1) ||
        (users_num == 0 && cu_ind() == 0)) { # only user 1 may add other users (user 1 is added by user 0)
      # browser()
      return(list(add_to_ds = TRUE,
                  nm = cmnd$args[[1]],
                  i1 = NA,
                  i2 = NA))
    } else {
      add_to_log_str(paste0('Only user ',
                            (dataset() %>% filter(cmd == 'nu'))$nm[[1]],
                            ' can add new users!'), 
                     'wrng')
      return(list(add_to_ds = FALSE))
    }
  }
  
  check_pw <- function(c_a) {
    # browser()
    if(as.numeric(c_a[[1]]) == 0) return(TRUE)
    nm <- (dataset() %>% filter(cmd == 'nu'))$nm[[as.numeric(c_a[[1]])]]
    if (c_a[[2]] == substr(nm, 1, 3)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  run_command.cu <- function(cmnd) {
    if (!check_pw(cmnd$args)) {
      add_to_log_str(paste0('Wrong!'), 'wrng')
      return(list(add_to_ds = FALSE))
    }
    # browser()
    cu_ind_nr <- as.numeric(cmnd$args[[1]]) # chosen user's ind
    if(is.na(cu_ind_nr)) {
      add_to_log_str(paste0(cmnd$args[[1]], ' is not numeric!'), 'wrng')
      return(list(add_to_ds = FALSE))
    }
    users_num <- dataset() %>% filter(cmd == 'nu') %>% nrow()
    current_users_nr <- current_users()
    if (cu_ind_nr != 0) { # if the user is not signing out
      # browser()
      if (current_users_nr %>% filter(u_ind == cu_ind_nr) %>% nrow() > 0) { # if someone has already signed in as cu_ind
        # browser()
        if (current_users_nr$u_gnm[current_users_nr$u_ind == cu_ind_nr] == cu_gnm) {
          add_to_log_str(paste0('You  have already signed in as ', cu_ind_nr, '!'), 
                         'wrng')
        } else {
          add_to_log_str(paste0('Somebody has already signed in as user ', cu_ind_nr, '!'), 
                         'wrng')
        }
        return(list(add_to_ds = FALSE))
      }
    }
    # browser()
    if (cu_ind_nr == 1) { # if the user signing in as admin
      insertUI(
        # multiple = TRUE,
        selector = '#firstRow',
        where = 'beforeBegin',
        ui = fluidRow(id = 'admin_console', 
                      actionButton('undo', 'Undo'),
                      actionButton('redo', 'Redo'))
      )
    } else {
      removeUI(
        selector = '#admin_console',
      )
    }
    current_users_nr$u_ind[current_users_nr$u_gnm == cu_gnm] <- cu_ind_nr
    cu_ind(cu_ind_nr)
    current_users(current_users_nr)
    return(list(add_to_ds = FALSE))
  }
  
  run_command.nc <- function(cmnd) {
    return(list(add_to_ds = TRUE,
                nm = cmnd$args[[1]],
                i1 = NA,
                i2 = NA))
  }
  
  run_command.ns <- function(cmnd) {
    return(list(add_to_ds = TRUE,
                nm = cmnd$args[[1]],
                i1 = NA,
                i2 = NA))
  }
  
  # cmd date/time
  run_command.uc <- function(cmnd) {
    cu_uc_ds <- dataset() %>% # all countries of current user
      filter(u_i == cu_ind(), cmd == 'uc')
    cu_uc_num <- nrow(cu_uc_ds) # how many countries current user mentioned
    if (cu_uc_num > 0) {
      # browser()
      c_ind <- as.numeric(cu_uc_ds$nm[[cu_uc_num]])
      c_nm <- (dataset() %>% filter(cmd == 'nc'))$nm[[1]] # country name by ind
      add_to_log_str(paste0('The last country you mentioned was ', 
                            c_nm, 
                            '. To change it use command ... .'), 
                     'wrng')
      return(list(add_to_ds = FALSE))
    }
    return(list(add_to_ds = TRUE,
                nm = NA,
                i1 = as.numeric(cmnd$args[[1]]),
                i2 = NA))
  }
# supporting run_command()
#===============================================================================

#===============================================================================
# supporting undo_command()
  undo_command <- function(cmnd) {
    UseMethod("run_command")
  }
  
  undo_command <- function(cmnd) {
    add_to_log_str(paste0('Command "', class(cmnd), '" (tried to be undone) is unknown!'), 
                   'wrng')
  }
  
  undo_command.nu <- function(cmd_ind) {
    if (cmd_ind != nrow(dataset)) {
      add_to_log_str(paste0(cmd_ind, ' is not the last command!'), 
                     'wrng')
      return()
    }
    
  }
# supporting undo_command()
#===============================================================================
  
}
