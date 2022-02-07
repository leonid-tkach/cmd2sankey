dataset <- reactiveVal(tibble(
  cmd = character(), # nu (new user), nc (new country), ns (new state)
  cmd_ind = numeric(), # command index (user runs commands one by one)
  u_nm = character(),
  c_nm = character(),
  s_nm = character()
))

commands <- reactiveVal(tribble(
  ~cmd, ~args,
  "nu", c('u_nm'), # new user
  "nc", c('c_nm'), # new country
  "ns", c('s_nm') # new state
))

cmd_log <- reactiveVal(tibble(
  cmd_ind = numeric(),
  cmd = character(),
  args = character()
))

function(input, output, session) {
  command <- reactiveVal('')
  log_str <- reactiveVal('')
  
  output$dataset <- renderReactable({
    reactable(dataset())
  })

  output$cmd_log <- renderReactable({
    reactable(cmd_log())
  })
  
  output$log <- renderText({
    log_str()
  })
  
  observeEvent(list(input$keys, input$run), {
    # browser()
    command(input$tmnl)
  })
  
  observeEvent(command(), {
    log_str(paste0(command(), '\n',log_str()))
    isolate(updateTextInput(session, 'tmnl', value = ''))
    cmnd_vec <- str_extract_all(command(), '(\\w+)') %>% 
      unlist()
    cmnd <- structure(class = cmnd_vec[1],
                      list(args = cmnd_vec[-1]))
    run_command(cmnd)
  })
  
  run_command <- function(cmnd) {
    UseMethod("run_command")
  }
  
  run_command.default <- function(cmnd) {
    log_str(paste0('Command "', class(cmnd), '" is unknown!\n', log_str()))
  }
  
  run_command.nu <- function(cmnd) {
    # browser()
    cur_cmd_ind = nrow(cmd_log()) + 1
    dataset(isolate(
      dataset() %>% 
        add_row(cmd_ind = cur_cmd_ind,
                cmd = 'nu',
                u_nm = cmnd$args[[1]]
        )
    ))
    
    cmd_log(isolate(
      cmd_log() %>% 
        add_row(cmd_ind = cur_cmd_ind,
                cmd = class(cmnd),
                args = paste(cmnd$args, collapse = ' ')
        )
    ))
  }
  
}
