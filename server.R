#===============================================================================
# global variables

load('dataset.RData')

dataset <- reactiveVal(dataset_nr)

rm(dataset_nr)

# dataset <- reactiveVal(tibble(
#   cmd_ind = numeric(), # command index (user runs commands one by one)
#   cmd = character(), # nu (new user), nc (new country), ns (new state)
#   u_ind = numeric(),
#   u_nm = character(),
#   c_nm = character(),
#   s_nm = character()
# ))

commands <- reactiveVal(tribble(
  ~cmd, ~args,
  'nu', c('u_nm'), # new user
  'cu', c('u_ind', 
          '_pw'), # choose user (u_ind is user's indicator in dataset, one by one
                  # arg with a name starting with '_' does not have its col in dataset
  'nc', c('c_nm'), # new country
  'ns', c('s_nm') # new state
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
    reactable(dataset() %>% arrange(desc(cmd_ind)))
  })
  
  output$current_users <- renderReactable({
    reactable(current_users())
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
    
    run_command(cmnd)
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
      dataset(isolate(
        dataset() %>% 
          add_row(u_ind = cu_ind(),
                  cmd_ind = nrow(dataset()) + 1,
                  cmd = 'nu',
                  u_nm = cmnd$args[[1]]
          )
      ))
      
    } else {
      add_to_log_str(paste0('Only user ',
                            (dataset() %>% filter(cmd == 'nu'))$u_nm[[1]],
                            ' may add new users!'), 
                     'wrng')
    }
  }
  
  check_pw <- function(c_a) {
    # browser()
    if(as.numeric(c_a[[1]]) == 0) return(TRUE)
    nm <- (dataset() %>% filter(cmd == 'nu'))$u_nm[[as.numeric(c_a[[1]])]]
    if (c_a[[2]] == substr(nm, 1, 3)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  run_command.cu <- function(cmnd) {
    if (!check_pw(cmnd$args)) {
      add_to_log_str(paste0('Wrong!'), 
                     'wrng')
      return()
    }
    # browser()
    cu_ind_nr <- as.numeric(cmnd$args[[1]]) # chosen user's ind
    if(is.na(cu_ind_nr)) {
      add_to_log_str(paste0(cmnd$args[[1]], ' is not numeric!'), 
                     'wrng')
      return()
    }
    users_num <- dataset() %>% filter(cmd == 'nu') %>% nrow()
    current_users_nr <- current_users()
    if (cu_ind_nr != 0) { # if the user is not signing out
      if (cu_ind_nr > users_num) {
        add_to_log_str(paste0('There is no user ', cu_ind_nr, 
                              '! There are only ', users_num, ' users.'), 
                       'wrng')
        return()
      }
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
        return()
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
  }
  
  run_command.nc <- function(cmnd) {
    dataset(isolate(
      dataset() %>% 
        add_row(u_ind = cu_ind(),
                cmd_ind = nrow(dataset()) + 1,
                cmd = 'nc',
                c_nm = cmnd$args[[1]]
        )
    ))
  }
  
  run_command.ns <- function(cmnd) {
    dataset(isolate(
      dataset() %>% 
        add_row(u_ind = cu_ind(),
                cmd_ind = nrow(dataset()) + 1,
                cmd = 'ns',
                s_nm = cmnd$args[[1]]
        )
    ))
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
