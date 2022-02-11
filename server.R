#===============================================================================
# global variables

# load('datasets.RData')
dataset <- reactiveVal(tibble(
  cmd = character(), # nu (new user), nc (new country), ns (new state)
  cmd_ind = numeric(), # command index (user runs commands one by one)
  u_nm = character(),
  c_nm = character(),
  s_nm = character()
))

commands <- reactiveVal(tribble(
  ~cmd, ~args,
  'nu', c('u_nm'), # new user
  'cu', c('u_ind'), # choose user (u_ind is not existing argument but
                    # user's indicator in dataset, one by one)
  'nc', c('c_nm'), # new country
  'ns', c('s_nm') # new state
))

cmd_log <- reactiveVal(tibble(
  cmd_ind = numeric(),
  cmd = character(),
  args = character()
))

current_users <- reactiveVal(tibble(
  u_gnm = character(), # user's generated unique name
  u_ind = numeric() # index number of a known user (from dataset)
))

cur_cmd_ind <- reactiveVal(nrow(cmd_log))

# global variables
#===============================================================================

function(input, output, session) {
  command <- reactiveVal('')
  log_str <- reactiveVal('')
  
#===============================================================================
# supporting current users()
  cu_gnm <- reactiveVal('') # current user's generated unique name
  cu_ind <- reactiveVal(numeric()) # current user's indicator form current_users
  
  # browser()
  
  session_init <- FALSE
  
  session$onSessionEnded(function() {
    # browser()
    current_users(isolate({
      current_users() %>% filter(u_gnm != cu_gnm)
    }))
    
    # browser()
    save(dataset, 
         commands, 
         cmd_log, 
         file = 'datasets.RData',
         envir = environment())
  })
  
  if (!session_init) {
    # Seed username
    cu_gnm <- paste0("User", round(runif(1, 10000, 99999)))
    current_users(isolate(
      current_users() %>% 
        add_row(u_gnm = cu_gnm)
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
  
  output$cmd_log <- renderReactable({
    reactable(cmd_log() %>% arrange(desc(cmd_ind)))
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
  delete_last_row_in_cmd_log <- function() {
    cmd_log(isolate(
      head(cmd_log(), -1)
    ))
  }
  
  add_to_log_str <- function(str_to_add, cmd_wrng) {
    log_str(paste0(if_else(cmd_wrng == 'cmd', '> ', ''), 
                   str_to_add, '<br/>', 
                   log_str()))
  }
  
  observeEvent(command(), {
    add_to_log_str(command(), 'cmd')
    isolate(updateTextInput(session, 'tmnl', value = ''))
    cmnd_vec <- str_extract_all(command(), '(\\w+)') %>% 
      unlist()
    cmnd <- structure(class = cmnd_vec[1],
                      list(args = cmnd_vec[-1]))
    # browser()
    args_num <- length(commands()$args[commands()$cmd == class(cmnd)])
    if (length(cmnd$args) < args_num) {
      add_to_log_str(paste0('Command "', class(cmnd), '" has ',
                            args_num, ' args, but you have only provided ',
                            length(cmnd$args), ' args.'), 'wrng')
      return()
    }
    cmd_log(isolate(
      cmd_log() %>% 
        add_row(cmd_ind = nrow(cmd_log()) + 1,
                cmd = class(cmnd),
                args = paste(cmnd$args, collapse = ' ')
        )
    ))
    run_command(cmnd)
  })
  
  run_command <- function(cmnd) {
    UseMethod("run_command")
  }
  
  run_command.default <- function(cmnd) {
    delete_last_row_in_cmd_log()
    add_to_log_str(paste0('Command "', class(cmnd), '" is unknown!'), 
                   'wrng')
  }
  
  run_command.nu <- function(cmnd) {
    # browser()
    dataset(isolate(
      dataset() %>% 
        add_row(cmd_ind = nrow(cmd_log()),
                cmd = 'nu',
                u_nm = cmnd$args[[1]]
        )
    ))
  }
  
  run_command.cu <- function(cmnd) {
    # browser()
    cu_ind <- as.numeric(cmnd$args[[1]]) # chosen user's ind
    if(is.na(cu_ind)) {
      add_to_log_str(paste0(cmnd$args[[1]], ' is not numeric!'), 
                     'wrng')
      delete_last_row_in_cmd_log()
      return()
    }
    users_num <- dataset() %>% filter(cmd == 'nu') %>% nrow()
    current_users_nr <- current_users()
    if (cu_ind > users_num) {
      add_to_log_str(paste0('There is no user ', cu_ind, 
                            '! There are only ', users_num, ' users.'), 
                     'wrng')
      delete_last_row_in_cmd_log()
      return()
    }
    # browser()
    if (current_users_nr %>% filter(u_ind == cu_ind) %>% nrow() > 0) {
      if (current_users_nr$u_gnm[current_users_nr$u_ind == cu_ind] == cu_gnm) {
        add_to_log_str(paste0('You  have already signed in as ', cu_ind, '!'), 
                       'wrng')
      } else {
        add_to_log_str(paste0('Somebody has already signed in as user ', cu_ind, '!'), 
                       'wrng')
      }
      delete_last_row_in_cmd_log()
      return()
    }
    current_users_nr$u_ind[current_users_nr$u_gnm == cu_gnm] <- cu_ind
    current_users(current_users_nr)
    # browser()
    if (cu_ind == 1) {
      insertUI(
        # multiple = TRUE,
        selector = '#firstRow',
        where = 'beforeBegin',
        ui = fluidRow(id = 'admin_console', actionButton('undo', 'Undo'))
      )
    } else {
      removeUI(
        selector = '#admin_console',
      )
    }
  }
# supporting run_command()
#===============================================================================

#===============================================================================
# supporting undo_run_command()
  undo_run_command.cu <- function(cmd_ind) {
    # if_else ()
  }
# supporting undo_run_command()
#===============================================================================
  
}
