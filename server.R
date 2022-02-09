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

function(input, output, session) {
  command <- reactiveVal('')
  log_str <- reactiveVal('')
  
  #=============================================================================
  # controlling current users()
  cu_gnm <- reactiveVal('') # current user's generated unique name
  cu_ind <- reactiveVal(numeric()) # current user's indicator form current_users
  
  # browser()
  
  session_init <- FALSE
  
  session$onSessionEnded(function() {
    # browser()
    current_users(isolate({
      current_users() %>% filter(u_gnm != cu_gnm)
    }))
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
  
  # controlling current users()
  #=============================================================================
  
  output$dataset <- renderReactable({
    reactable(dataset() %>% arrange(desc(cmd_ind)))
  })
  
  output$current_users <- renderReactable({
    reactable(current_users())
  })
  
  output$cmd_log <- renderReactable({
    reactable(cmd_log() %>% arrange(desc(cmd_ind)))
  })
  
  output$log <- renderText({
    log_str()
  })
  
  observeEvent(list(input$keys, input$run), {
    # browser()
    command(input$tmnl)
  })
  
  delete_last_row_in_cmd_log <- function() {
    cmd_log(isolate(
      head(cmd_log(), -1)
    ))
  }
  
  add_to_log_str <- function(str_to_add) {
    log_str(paste0(str_to_add, '\n', log_str()))
  }
  
  observeEvent(command(), {
    add_to_log_str(command())
    isolate(updateTextInput(session, 'tmnl', value = ''))
    cmnd_vec <- str_extract_all(command(), '(\\w+)') %>% 
      unlist()
    cmnd <- structure(class = cmnd_vec[1],
                      list(args = cmnd_vec[-1]))
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
    add_to_log_str(paste0('Command "', class(cmnd), '" is unknown!'))
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
    users_num <- nrow(dataset() %>% filter(cmd == 'nu'))
    current_users_nr <- current_users()
    if (cmnd$args[[1]] > users_num) {
      add_to_log_str(paste0('There is no user ', cmnd$args[[1]], 
                            '! There are only ', users_num, ' users.'))
      delete_last_row_in_cmd_log()
      return()
    }
    current_users_nr$u_ind[current_users_nr$u_gnm == cu_gnm] <- cmnd$args[[1]]
    current_users(current_users_nr)
  }
  
}
