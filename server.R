#_global variables==============================================================

load('dataset.RData')

dataset <- reactiveVal(dataset_nr)
cur_cmd_i <- reactiveVal(cur_cmd_i_nr)

rm(dataset_nr)
rm(cur_cmd_i_nr)

# dataset <- reactiveVal(tibble(
#   cmd_i = numeric(), # command index (user runs commands one by one)
#   cmd = character(), # command name
#   u_i = numeric(), # current user who run the command
#   nm = character(),
#   i1 = numeric(),
#   i2 = numeric(),
#   dt = as_datetime(numeric(), tz = 'EST'),
#   m = character(),
#   d = numeric(),
#   t = character()
# ))
# 
# cur_cmd_i <- reactiveVal(0)

commands <- reactiveVal(tribble(
  ~cmd, ~args,
  'nu', c('nm'), # new user
  'cu', c('i1', '_pw'), # choose user (i1 is user's indicator in dataset, one by one
                        # arg with a name starting with '_' does not have its col in dataset
  'nc', c('nm'), # new country
  'ns', c('nm'), # new state
  'uc', c('i1'), # user's country; i1 is county indicator in dataset, one by one
  'u', c(), # undo last command of current user
  'rn', c('cmd_i', '_nnm') # new name
                            # arg with a name starting with '_' does not have its col in dataset
))

current_users <- reactiveVal(tibble(
  u_ind = numeric(), # index number of a known user (from dataset)
  u_gnm = character() # user's generated unique name
))

#-global variables==============================================================

function(input, output, session) {
  command <- reactiveVal('')
  log_str <- reactiveVal('')
  
#_supporting current users()====================================================
  
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
    cur_cmd_i_nr <- isolate(cur_cmd_i())
    save(dataset_nr, 
         cur_cmd_i_nr, 
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
  
#-supporting current users()====================================================
  
#_supporting ui=================================================================
  
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
  
#-supporting ui=================================================================

#_supporting run_command()======================================================
  
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
  
  launch_undo <- function() {
    # browser()
    cuser_rows <- isolate(
      dataset() %>% 
        filter(u_i == cu_ind())
    )
    cuser_rows_num <- nrow(cuser_rows)
    if (cuser_rows_num == 0) {
      add_to_log_str(paste0('User ',
                            (dataset() %>% filter(cmd_i == cu_ind()))$nm,
                            ' does not have any commands to undo!'), 
                     'wrng')
      return()
    }
    cuser_last_cmd_row <- cuser_rows %>% 
      arrange(cmd_i) %>% 
      tail(1)
    # browser()
    cmnd <- structure(class = cuser_last_cmd_row$cmd[[1]],
                      list(cmd_i = cuser_last_cmd_row$cmd_i[[1]]))
    undo_command(cmnd)
  }
  
  launch_rename <- function(cmnd_vec) {
    rn_cmd_i <- as.numeric(cmnd_vec[[2]])
    if (is.na(rn_cmd_i)) {
      add_to_log_str(paste0('"', cmnd_vec[[2]], '" is not numeric!'), 
                     'wrng')
      return()
    }
    the_cmd <- dataset() %>% 
      filter(cmd_i == rn_cmd_i)
    if (nrow(the_cmd) == 0) {
      add_to_log_str("There is no command with such a cmd_i you are trying to rename!", 
                     'wrng')
      return()
    }
    if (the_cmd$u_i != cu_ind() && (cu_ind() != 1 || the_cmd$u_i != 0)) {
      add_to_log_str(paste0('Only user ', 
                            (dataset() %>% filter(cmd_i == the_cmd$u_i))$nm,
                            ' may rename this element!'), 
                     'wrng')
      return()
    }
    if (is.na(the_cmd$nm)) {
      add_to_log_str(paste0('Command ', the_cmd$cmd_i, ':', the_cmd$cmd, ' does not use "nm".'), 
                     'wrng')
      return()
    }
    dataset_nr <- dataset()
    dataset_nr$nm[dataset_nr$cmd_i == rn_cmd_i] <- paste(cmnd_vec[c(-1, -2)], collapse = ' ')
    dataset(dataset_nr)
  }
  
  observeEvent(command(), {
    # browser()
    if (command() == '') {
      return()
    }
    if(!str_detect(command(), "^[a-zA-Z0-9_\\-\\' ]+$")) {
      add_to_log_str("Unfortunately, some characters are unrecognizable. 
                     Could you please use only characters a-z, A_Z, _, -, ', and space?", 
                     'wrng')
      isolate(updateTextInput(session, 'tmnl', value = ''))
      command(isolate(''))
      return()
    }
    add_to_log_str(command(), 'cmd')
    isolate(updateTextInput(session, 'tmnl', value = ''))
    cmnd_vec <- str_extract_all(command(), '(\\w+)') %>% 
      unlist()
    command(isolate('')) # to enable two similar commands in seq
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
    if (cmnd_vec[1] == 'u') {
      launch_undo()
      return()
    }
    if (cmnd_vec[1] == 'rn') {
      launch_rename(cmnd_vec)
      return()
    }
    rc_res <- run_command(cmnd)
    if (rc_res$add_to_ds) { # cmnd is ok to add to dataset
      now_dt <- now(tzone = 'EST')
      cur_cmd_i(isolate(cur_cmd_i() + 1))
      dataset(isolate(
        dataset() %>% 
          add_row(u_i = cu_ind(),
                  cmd_i = cur_cmd_i(),
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
    return(list(add_to_ds = FALSE))
  }
  
  run_command.nu <- function(cmnd) {
    # browser()
    users_num <- dataset() %>% filter(cmd == 'nu') %>% nrow()
    if ((users_num > 0 && cu_ind() == 1) ||
        (users_num == 0 && cu_ind() == 0)) { # only user 1 may add other users (user 1 is added by user 0)
      # browser()
      return(list(add_to_ds = TRUE,
                  nm = paste(cmnd$args, collapse = ' '),
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
    u_cmd_i <- as.numeric(c_a[[1]])
    if(u_cmd_i == 0) return(TRUE)
    signing_in_user_row <- dataset() %>% filter(cmd_i == u_cmd_i)
    if (nrow(signing_in_user_row) == 0) {
      add_to_log_str(paste0('There is no user with cmd_i ', u_cmd_i, '!'), 'wrng')
      return(FALSE)
    }
    nm <- signing_in_user_row$nm
    if (c_a[[2]] == substr(nm, 1, 3)) {
      return(TRUE)
    } else {
      add_to_log_str(paste0('Wrong!'), 'wrng')
      return(FALSE)
    }
  }
  
  run_command.cu <- function(cmnd) {
    # browser()
    if (!check_pw(cmnd$args)) {
      return(list(add_to_ds = FALSE))
    }
    # browser()
    cu_ind_nr <- as.numeric(cmnd$args[[1]]) # chosen user's ind
    if(is.na(cu_ind_nr)) {
      add_to_log_str(paste0(cmnd$args[[1]], ' is not numeric!'), 'wrng')
      return(list(add_to_ds = FALSE))
    }
    # browser()
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
        selector = '#admin_console'
      )
    }
    current_users_nr$u_ind[current_users_nr$u_gnm == cu_gnm] <- cu_ind_nr
    cu_ind(cu_ind_nr)
    current_users(current_users_nr)
    return(list(add_to_ds = FALSE))
  }
  
  run_command.nc <- function(cmnd) {
    return(list(add_to_ds = TRUE,
                nm = paste(cmnd$args, collapse = ' '),
                i1 = NA,
                i2 = NA))
  }
  
  run_command.ns <- function(cmnd) {
    return(list(add_to_ds = TRUE,
                nm = paste(cmnd$args, collapse = ' '),
                i1 = NA,
                i2 = NA))
  }
  
  runner_uc_us <- function(cmnd,
                           cs) { # user country or state ('c' or 's')
    ncs <- paste0('n', class(cmnd) %>% substr(2, 2))
    cs_word <- if_else(ncs == 'nc', 'country', 'state')
    cu_ucs_ds <- dataset() %>% # all countries/states of current user
      filter(u_i == cu_ind(), cmd == class(cmnd))
    cu_ucs_num <- nrow(cu_ucs_ds) # how many countries/states current user mentioned
    if (cu_ucs_num > 0) {
      # browser()
      add_to_log_str(paste0('You have already set your ', cs_word, '. It is ', 
                            (dataset() %>% filter(cmd_i == cu_ucs_ds$i1))$nm, 
                            '. To change it use the command ... .'), 
                     'wrng')
      return(list(add_to_ds = FALSE))
    }
    cs_cmd_i <- as.numeric(cmnd$args[[1]])
    if((dataset() %>%
        filter(cmd == ncs, cmd_i == cs_cmd_i) %>% 
        nrow()) == 0) {
      add_to_log_str(paste0('There is no such ', cs_word, ' in the dataset!'), 
                     'wrng')
      return(list(add_to_ds = FALSE))
    } else {
      return(list(add_to_ds = TRUE,
                  nm = NA,
                  i1 = cs_cmd_i,
                  i2 = NA))
    }
  }
  
  run_command.uc <- function(cmnd) {
    runner_uc_us(cmnd, 'c')
  }
  
  run_command.us <- function(cmnd) {
    runner_uc_us(cmnd, 'c')
  }
  
#-supporting run_command()======================================================
  
#_supporting undo_command()=====================================================
  
  undo_command <- function(cmnd) {
    UseMethod("undo_command")
  }
  
  undo_command.default <- function(cmnd) {
    add_to_log_str(paste0('The command "', class(cmnd), '" you tried to undo is unknown!'), 
                   'wrng')
  }
  
  undo_command.nu <- function(cmnd) {
    # browser()
    d_user_commands <- dataset() %>% # commands launched by the user to be deleted
      filter(u_i == cmnd$cmd_i)
    if (nrow(d_user_commands) > 0) {
      add_to_log_str(paste0('There are commands launched by the user ',
                            cmnd$nm,
                            ' you are trying to delete: ', 
                            paste(d_user_commands %>% 
                                    select(cmd_i, cmd) %>% 
                                    pmap(function(cmd_i, cmd) paste(cmd_i, cmd, collapse = ' ')),
                                  collapse = ', '), 
                            '.'),
                     'wrng')
    } else {
      dataset(isolate(dataset() %>% 
                        filter(cmd_i != cmnd$cmd_i))
      )
    }
  }

  undoer_nc_ns <- function(cmnd) {
    ncs <- class(cmnd)
    cs_word <- if_else(ncs == 'nc', 'country', 'state')
    d_country_commands <- dataset() %>% # commands using the country to be deleted
      filter(i1 == cmnd$cmd_i)
    if (nrow(d_country_commands) > 0) {
      add_to_log_str(paste0('There are commands using ', cs_word, ' "',
                            (dataset() %>% filter(cmd_i == cmnd$cmd_i))$nm,
                            '" you are trying to delete: ', 
                            paste(d_country_commands %>% 
                                    select(u_i, cmd_i, cmd) %>% 
                                    pmap(function(u_i_a, cmd_i, cmd) {
                                      paste((dataset() %>% 
                                               filter(cmd_i == u_i_a))$nm, 
                                            ': ',
                                            cmd_i, 
                                            cmd, 
                                            collapse = ' ')
                                    }),
                                  collapse = ', '), 
                            '.'),
                     'wrng')
    } else {
      dataset(
        isolate(dataset() %>% 
                  filter(cmd_i != cmnd$cmd_i))
      )
    }
  }
  
  undo_command.nc <- function(cmnd) {
    undoer_nc_ns(cmnd)
  }

  undo_command.ns <- function(cmnd) {
    undoer_nc_ns(cmnd)
  }

  undo_command.uc <- function(cmnd) {
    dataset(
      isolate(dataset() %>% 
                filter(cmd_i != cmnd$cmd_i))
    )
  }
  
  undo_command.us <- function(cmnd) {
    dataset(
      isolate(dataset() %>% 
                filter(cmd_i != cmnd$cmd_i))
    )
  }
  
#-supporting undo_command()=====================================================
  
}
