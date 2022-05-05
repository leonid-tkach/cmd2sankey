#_global variables==============================================================

load('dataset.RData') # load dataset_nr, cur_cmd_i_nr

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

fig <- reactiveVal(list( # in the middle between dataset() and snk_fig()
  node = tibble(id = character(),
                label = character(),
                color = character()),
  link = tibble(source = numeric(),
                target = numeric(),
                value = numeric())))

snk_fig <- reactiveVal()

commands <- reactiveVal(tribble( # terminal commands & args
  ~cmd, ~args,
  'nu', c('nm'), # new user
  'cu', c('i1', '_pw'), # choose user (i1 is user's indicator in dataset and current_users, one by one
                        # arg with a name starting with '_' does not have its col in dataset
  'nc', c('nm'), # new country
  'ns', c('nm'), # new state
  'uc', c('i1'), # user's country; i1 is county indicator in dataset, one by one
  'u', c(), # undo last command of current user
  'rn', c('cmd_i', '_nnm'), # new name
                            # arg with a name starting with '_' does not have its col in dataset
  'nn', c('nm'), # new node named nm
  'nl', c('i1', 'i2', 'nm') # new link named nm from node cmd_i==i1 to node cmd_i==i2
))

current_users <- reactiveVal(tibble( # used in session$onSessionEnded()
  u_ind = numeric(), # index number of a known user (from dataset)
  u_gnm = character() # user's generated unique name
))

#-global variables==============================================================

function(input, output, session) {
  command <- reactiveVal('') # changed in observeEvent(list(input$keys, input$run), {
  log_str <- reactiveVal('') # printed in output$log <- renderUI({
  
#_supporting current users()====================================================
  
  cu_gnm <- reactiveVal('') # current user's generated unique name
  cu_ind <- reactiveVal(0) # current user's indicator from current_users and dataset
  
  session_init <- FALSE # this means: there is no new user yet
  
  session$onSessionEnded(function() {
    current_users(isolate({
      current_users() %>% filter(u_gnm != cu_gnm)
    })) # delete cu unique name from current_users before the user leaves the session
    
    dataset_nr <- isolate(dataset()) # save current version to _nr
    cur_cmd_i_nr <- isolate(cur_cmd_i()) # save current version to _nr
    save(dataset_nr, 
         cur_cmd_i_nr, 
         file = 'dataset.RData',
         envir = environment()) # save current data to dataset.RData
  })
  
  if (!session_init) { # if new user opened web app in browser
    # Seed username
    cu_gnm <- paste0("User", round(runif(1, 10000, 99999))) # generate new user's unique name
    current_users(isolate( # add new user to current_users
      current_users() %>% 
        add_row(u_gnm = cu_gnm,
                u_ind = 0)
    ))
    session_init <- TRUE # this means: the new user is here
  }
  
#-supporting current users()====================================================
  
#_supporting ui=================================================================
  
  output$dataset <- renderReactable({
    reactable(dataset() %>% 
                arrange(desc(cmd_i)) %>% # newest command first
                select(-dt), # no date  column
              defaultColDef = colDef(minWidth = 50)) # https://glin.github.io/reactable/articles/examples.html
  })

  output$nodes <- renderReactable({
    reactable(fig()$node,
              defaultColDef = colDef(minWidth = 50)) # https://glin.github.io/reactable/articles/examples.html
  })
  
  output$links <- renderReactable({
    reactable(fig()$link,
              defaultColDef = colDef(minWidth = 50)) # https://glin.github.io/reactable/articles/examples.html
  })
  
  output$snk_nodes <- renderReactable({
    reactable(snk_fig()$node,
              defaultColDef = colDef(minWidth = 50)) # https://glin.github.io/reactable/articles/examples.html
  })
  
  output$snk_links <- renderReactable({
    reactable(snk_fig()$link,
              defaultColDef = colDef(minWidth = 50)) # https://glin.github.io/reactable/articles/examples.html
  })
  
  output$current_users <- renderReactable({
    reactable(current_users(),
              defaultColDef = colDef(minWidth = 50)) # https://glin.github.io/reactable/articles/examples.html
  })
  
  output$log <- renderUI({
    HTML(log_str()) # update log window https://shiny.rstudio.com/reference/shiny/1.6.0/HTML.html
  })
  
  observeEvent(list(input$keys, # look in ui (keysInput('keys', c('enter'), global = TRUE))
                                # read command if 'enter' pressed
                                # https://rdrr.io/cran/keys/man/keysInput.html
                    input$run), { # read command if actbtn 'run' pressed
    # browser()
    command(input$tmnl) # change command to string in terminal
  })
  
#-supporting ui=================================================================

#_utils=========================================================================

  val_as_numeric <- function(num_val_in_char) {
    # check if num (log it, if not), convert to num, init "ok"-attr of returned val with true or false
    num_val <- as.numeric(num_val_in_char)
    if (is.na(num_val)) {
      add_to_log_str(paste0('"', num_val_in_char, '" is not numeric!'), 
                     'wrng')
      attr(num_val, "ok") <- FALSE
      return(num_val)
    }
    attr(num_val, "ok") <- TRUE
    return(num_val)
  }
  
  row_by_cmd_i <- function(cmd_i_a, txt_before, txt_after, cmd_a = NA) {
    # return row by command index; if there is now such cmd_i, log it concatenating (txt_before, cmd_i, txt_after);
    # if cmd_a (nc, ns) provided, it searched only among 'nc' or 'ns' ('new country', 'new state');
    # init "ok"-attr of returned val with true or false
    if(is.na(cmd_a)) the_row <- dataset() %>% filter(cmd_i == cmd_i_a)
    else the_row <- dataset() %>% filter(cmd_i == cmd_i_a, cmd == cmd_a) # to searching among commands cmd_a
    
    if (nrow(the_row) == 0) {
      add_to_log_str(paste0(txt_before, as.character(cmd_i_a), txt_after), 
                     'wrng')
      attr(the_row, "ok") <- FALSE
      return(the_row)
    }
    attr(the_row, "ok") <- TRUE
    return(the_row)
  }
  
  check_pw <- function(c_a) {
    # check password correctness;
    # currently it must first three letters of the sign-ining user' name
    u_cmd_i <- val_as_numeric(c_a[[1]])
    if (!attr(u_cmd_i, "ok")) return(FALSE)
    if(u_cmd_i == 0) return(TRUE)
    
    signing_in_user_row <- row_by_cmd_i(u_cmd_i, 'There is no user with cmd_i=', '!')
    if (!attr(signing_in_user_row, "ok")) return(FALSE)
    
    nm <- signing_in_user_row$nm
    if (c_a[[2]] == substr(nm, 1, 3)) {
      return(TRUE)
    } else {
      add_to_log_str(paste0('Wrong!'), 'wrng')
      return(FALSE)
    }
  }
  
#-utils=========================================================================
  
#_supporting run_command()======================================================
  
  delete_last_row_in_dataset <- function() {
    dataset(isolate(
      head(dataset(), -1)
    ))
  }
  
  add_to_log_str <- function(str_to_add, cmd_wrng = 'wrng') {
    # if command, print '>' first
    log_str(paste0(if_else(cmd_wrng == 'cmd', '> ', ''), 
                   str_to_add, '<br/>', 
                   log_str()))
  }
  
  launch_undo <- function(cmnd_vec) {
    # used in observeEvent(command(), {
    if (length(cmnd_vec) == 1) { # 'no arguments' means 'undo last command of this user (cu_ind)'
      cuser_rows <- isolate( # filter to current user's commands
        dataset() %>% 
          filter(u_i == cu_ind())
      )
      cuser_rows_num <- nrow(cuser_rows)
      if (cuser_rows_num == 0) { # if there no current user's commands, log the fact
        add_to_log_str(paste0('User ',
                              (dataset() %>% filter(cmd_i == cu_ind()))$nm,
                              ' does not have any commands to undo!'), 
                       'wrng')
        return()
      }
      cmd_to_del_row <- cuser_rows %>% # find last cu's command
        arrange(cmd_i) %>% 
        tail(1)
    } else { # the only possible argument is cmd_i of the command to undo
      cmd_to_del_i <- val_as_numeric(cmnd_vec[2]) # check if provided cmd_i is numeric
      if (!attr(cmd_to_del_i, "ok")) return()
      cmd_to_del_row <- row_by_cmd_i(cmd_to_del_i,
                                     'There is no command with cmd_i=',
                                     ' you are trying to undo!')
      if (!attr(cmd_to_del_row, "ok")) return() # if there is no such cmd_i
      # browser()
      if (cmd_to_del_row$u_i != as.numeric(cu_ind())) {
        add_to_log_str(paste0('Only user ', 
                              (dataset() %>% filter(cmd_i == cmd_to_del_row$u_i))$nm,
                              ' may undo this command!'), 
                       'wrng')
        return()
      }
    }
    cmnd <- structure(class = cmd_to_del_row$cmd[[1]], # make a cmnd-structure of it
                      list(cmd_i = cmd_to_del_row$cmd_i[[1]]))
    undo_command(cmnd) # opposite to run_command (there is one for every command too)
  }

  launch_rename <- function(cmnd_vec) {
    # used in observeEvent(command(), {
    rn_cmd_i <- val_as_numeric(cmnd_vec[[2]]) # check if provided cmd_i is numeric
    if (!attr(rn_cmd_i, "ok")) return()
    
    the_cmd <- row_by_cmd_i(rn_cmd_i, 'There is no element with cmd_i=', ' you are trying to rename!')
    if (!attr(the_cmd, "ok")) return() # if there is no such cmd_i
    
    # only command's author can rename it
    # if it is the very first command, it's u_i is 0 (not 1), because it is the very first user added themselves
    # so user with u_i == 1 cand rename it
    if (the_cmd$u_i != cu_ind() && (cu_ind() != 1 || the_cmd$u_i != 0)) {
      add_to_log_str(paste0('Only user ', 
                            (dataset() %>% filter(cmd_i == the_cmd$u_i))$nm,
                            ' may rename this element!'), 
                     'wrng')
      return()
    }
    if (is.na(the_cmd$nm)) { # quit, if the_cmd does not use name (as cu, uc, rn commands)
      add_to_log_str(paste0('Command ', the_cmd$cmd_i, ':', the_cmd$cmd, ' does not use "nm".'), 
                     'wrng')
      return()
    }
    dataset_nr <- dataset() # change nm in non-reactive version of dataset
    # new name may consist of multiple words; I separate them with space
    dataset_nr$nm[dataset_nr$cmd_i == rn_cmd_i] <- paste(cmnd_vec[c(-1, -2)], collapse = ' ')
    dataset(dataset_nr) # copy it to reactive version
  }
  
  observeEvent(command(), {
    if (command() == '') { # command can't be empty
      return()
    }
    if(!str_detect(command(), "^[a-zA-Z0-9_\\-\\' ]+$")) { # command must by in Latin characters
      add_to_log_str("Unfortunately, some characters are unrecognizable. 
                     Could you please use only characters a-z, A_Z, _, -, ', and space?", 
                     'wrng')
      isolate(updateTextInput(session, 'tmnl', value = ''))
      command(isolate(''))
      return()
    }
    add_to_log_str(command(), 'cmd')
    isolate(updateTextInput(session, 'tmnl', value = '')) # empty command line
    cmnd_vec <- str_extract_all(command(), '(\\w+)') %>% # extract words from command
      unlist()
    command(isolate('')) # empty command var to enable two similar commands in seq
    if (cu_ind() == 0 && nrow(dataset()) > 0 && cmnd_vec[1] != 'cu') {
      # user cannot do anything before signing in
      # exception: user can add the very first user (him/herself)
      add_to_log_str( 'Please sign in!', 'wrng')
      return()
    }
    cmnd <- structure(class = cmnd_vec[1], # command name is its class; its args are in args
                      list(args = cmnd_vec[-1]))
    # count cmnd args according to commands-tibble
    args_num <- length(commands()$args[commands()$cmd == class(cmnd)] %>% unlist())
    if (length(cmnd$args) < args_num) { # if there are not enough args
      add_to_log_str(paste0('Command "', class(cmnd), '" has ',
                            args_num, ' args, but you have only provided ',
                            length(cmnd$args), ' args.'), 'wrng')
      return()
    }
    # commands not launched by run_command()
    if (cmnd_vec[1] == 'u') {
      launch_undo(cmnd_vec)
      return()
    }
    if (cmnd_vec[1] == 'rn') {
      launch_rename(cmnd_vec)
      return()
    }
    rc_res <- run_command(cmnd)
    if (rc_res$add_to_ds) { # if cmnd is ok to add to dataset
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
  
  run_command <- function(cmnd) { # S3
    UseMethod("run_command")
  }
  
  run_command.default <- function(cmnd) {
    add_to_log_str(paste0('Command "', class(cmnd), '" is unknown!'), 
                   'wrng')
    return(list(add_to_ds = FALSE))
  }
  
  run_command.nu <- function(cmnd) {
    # new user
    users_num <- dataset() %>% filter(cmd == 'nu') %>% nrow() # current users' num
    if ((users_num > 0 && cu_ind() == 1) ||
        (users_num == 0 && cu_ind() == 0)) { # only user 1 may add other users (user 1 is added by user 0)
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
  
  run_command.cu <- function(cmnd) {
    if (!check_pw(cmnd$args)) { # if password is wrong, ignore the command
      return(list(add_to_ds = FALSE))
    }
    chu_ind_nr <- val_as_numeric(cmnd$args[[1]]) # chosen user's ind
    if (!attr(chu_ind_nr, "ok")) return(list(add_to_ds = FALSE)) # if c_ind=cmnd$args[[1]] is not numeric, ignore the command
    current_users_nr <- current_users()
    if (chu_ind_nr != 0) { # if the user is not signing out
      if (current_users_nr %>% filter(u_ind == chu_ind_nr) %>% nrow() > 0) { # if chosen user ind is already among current_users
        if (current_users_nr$u_gnm[current_users_nr$u_ind == chu_ind_nr] == cu_gnm) {
          # if user with a chosen user ind has the same current user's gnm
          add_to_log_str(paste0('You  have already signed in as ', chu_ind_nr, '!'), 
                         'wrng')
        } else { #  if someone has already signed in as chu_ind
          add_to_log_str(paste0('Somebody has already signed in as user ', chu_ind_nr, '!'), 
                         'wrng')
        }
        return(list(add_to_ds = FALSE))
      }
    }
    if (chu_ind_nr == 1) { # if chosen user is an admin, add some ui
      insertUI(
        # multiple = TRUE,
        selector = '#firstRow',
        where = 'beforeBegin',
        ui = fluidRow(id = 'admin_console', 
                      actionButton('undo', 'Undo'),
                      actionButton('redo', 'Redo'))
      )
    } else { # if the user signing in as not an admin, remove some ui
      removeUI(
        selector = '#admin_console'
      )
    }
    current_users_nr$u_ind[current_users_nr$u_gnm == cu_gnm] <- chu_ind_nr
    # set [user with u_gnm == cu_gnm - current user]'s to chosen user' ind == cmnd$args[[1]]
    cu_ind(chu_ind_nr) # switch cur user's ind to chosen user ind
    current_users(current_users_nr)
    return(list(add_to_ds = FALSE)) # this command does not yield a useful line in dataset
  }
  
  run_command.nc <- function(cmnd) { # add country
    return(list(add_to_ds = TRUE,
                nm = paste(cmnd$args, collapse = ' '),
                i1 = NA,
                i2 = NA))
  }
  
  run_command.ns <- function(cmnd) { # add state
    return(list(add_to_ds = TRUE,
                nm = paste(cmnd$args, collapse = ' '),
                i1 = NA,
                i2 = NA))
  }
  
  runner_uc_us <- function(cmnd, # used in run_command.uc and run_command.us
                           cs) { # user country or state ('c' or 's')
    ncs <- paste0('n', class(cmnd) %>% substr(2, 2)) # ncs is 'nc' or 'ns'
    cs_word <- if_else(ncs == 'nc', 'country', 'state')
    cu_ucs_ds <- dataset() %>% # all countries/states of current user
      filter(u_i == cu_ind(), cmd == class(cmnd))
    cu_ucs_num <- nrow(cu_ucs_ds) # how many countries/states current user mentioned
    if (cu_ucs_num > 0) { # if current user already has country/state, quit
      add_to_log_str(paste0('You have already set your ', cs_word, '. It is ', 
                            (dataset() %>% filter(cmd_i == cu_ucs_ds$i1))$nm, 
                            '. To change it use the command ... .'), # todo
                     'wrng')
      return(list(add_to_ds = FALSE))
    }
    cs_cmd_i <- val_as_numeric(cmnd$args[[1]])
    if (!attr(cs_cmd_i, "ok")) return(list(add_to_ds = FALSE)) # if proposed country/state id is not numeric, quit
    signing_in_user_row <- row_by_cmd_i(cs_cmd_i, 
                                        paste0('There is no such ', cs_word, ' (cmd_i='), 
                                        ') in the dataset!', cmd_a = ncs)
    if (!attr(signing_in_user_row, "ok")) { # if there is no country/state with such id, quit
      return(list(add_to_ds = FALSE))
    } else {
      return(list(add_to_ds = TRUE,
                  nm = NA,
                  i1 = cs_cmd_i,
                  i2 = NA))
    }
  }
  
  run_command.uc <- function(cmnd) { # current user chooses their country
    runner_uc_us(cmnd, 'c')
  }
  
  run_command.us <- function(cmnd) { # current user chooses their state
    runner_uc_us(cmnd, 's')
  }
  
  run_command.nn <- function(cmnd) { # add new node
    return(list(add_to_ds = TRUE,
                nm = paste(cmnd$args, collapse = ' '),
                i1 = NA,
                i2 = NA))
  }
  
  run_command.nl <- function(cmnd) { # add new link
    node1_i <- row_by_cmd_i(cmnd$args[1], 'There is no node with cmd_i=', '!')
    if (!attr(node1_i, "ok")) return(FALSE)
    node2_i <- row_by_cmd_i(cmnd$args[2], 'There is no node with cmd_i=', '!')
    if (!attr(node2_i, "ok")) return(FALSE)
    # browser()
    return(list(add_to_ds = TRUE,
                nm = paste(cmnd$args[c(-1, -2)], collapse = ' '),
                i1 = node1_i$cmd_i,
                i2 = node2_i$cmd_i))
  }
  
#-supporting run_command()======================================================
  
#_supporting undo_command()=====================================================
  
  undo_command <- function(cmnd) { # S3
    UseMethod("undo_command")
  }
  
  undo_command.default <- function(cmnd) {
    add_to_log_str(paste0('The command "', class(cmnd), '" you tried to undo is unknown!'), 
                   'wrng')
  }
  
  undo_command.nu <- function(cmnd) {
    d_user_commands <- dataset() %>% # commands launched by the user to be deleted
      filter(u_i == cmnd$cmd_i)
    if (nrow(d_user_commands) > 0) { # if there are any - don't delete the user
      add_to_log_str(paste0('There are commands launched by the user ',
                            cmnd$nm,
                            ' you are trying to delete: ', 
                            paste(d_user_commands %>% 
                                    select(cmd_i, cmd) %>% 
                                    pmap(function(cmd_i, cmd) paste(cmd_i, cmd, collapse = ' ')),
                                  collapse = ', '), 
                            '.'),
                     'wrng')
    } else { # if there are no commands launched by the user - delete the user
      dataset(isolate(dataset() %>% 
                        filter(cmd_i != cmnd$cmd_i))
      )
    }
  }

  undoer_nc_ns <- function(cmnd) { # used in undo_command.nc and undo_command.ns
    ncs <- class(cmnd)
    cs_word <- if_else(ncs == 'nc', 'country', 'state')
    d_country_commands <- dataset() %>% # commands using the country/state to be deleted
      filter(i1 == cmnd$cmd_i)
    if (nrow(d_country_commands) > 0) { # if there are any - don't delete the country/state
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
    } else { # if there are no commands using the country/state - delete the the country/state
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
      isolate(dataset() %>% # filter out the command
                filter(cmd_i != cmnd$cmd_i))
    )
  }
  
  undo_command.us <- function(cmnd) {
    dataset(
      isolate(dataset() %>% # filter out the command
                filter(cmd_i != cmnd$cmd_i))
    )
  }
  
  undo_command.nn <- function(cmnd) {
    # browser()
    links2nn <- dataset() %>% # links to this node
      filter(cmd == 'nl' & (i1 == cmnd$cmd_i | i1 == cmnd$cmd_i))
    if (nrow(links2nn) > 0) {
      add_to_log_str('There are links to this node!')
    } else {
      dataset(
        isolate(dataset() %>% # filter out the command
                  filter(cmd_i != cmnd$cmd_i))
      )
    }
  }
  
  undo_command.nl <- function(cmnd) {
    dataset(
      isolate(dataset() %>% # filter out the command
                filter(cmd_i != cmnd$cmd_i))
    )
  }
  
#-supporting undo_command()=====================================================
  
#_supporting sankey=============================================================

  observeEvent(dataset(), { # extract "raw"  nodes and links from dataset()
    node_commands <- dataset() %>% 
      filter(cmd == 'nn') %>% 
      arrange(desc(cmd_i))
    link_commands <- dataset() %>% 
      filter(cmd == 'nl') %>% 
      arrange(desc(cmd_i))
    fig_nr <- fig()
    fig_nr$node <- tibble(id = node_commands$cmd_i,
                          label = node_commands$nm,
                          color = rep(NA, length(node_commands$cmd_i)))
    fig_nr$link <- tibble(source = link_commands$i1,
                          target = link_commands$i2,
                          label = link_commands$nm)
    fig(fig_nr)
  })  

  observeEvent(fig(), { # change "raw" node indexes from dataset() to positions for sankey
    if (nrow(fig()$node) > 0) {
      ind_pos <- tibble(ind = fig()$node$id,
                        pos = seq(from = 0, to = length(fig()$node$id) - 1))
      snk_fig_nr <- fig()
      snk_fig_nr$node <- snk_fig_nr$node %>% 
        select(-id)
      if (nrow(fig()$link) > 0) {
        source_ind <- fig()$link$source
        target_ind <- fig()$link$target
        snk_fig_nr$link <- tibble(source = ind_pos$pos[ind_pos$ind == source_ind],
                                  target = ind_pos$pos[ind_pos$ind == target_ind],
                                  label = fig()$link$label)
      }
      snk_fig(snk_fig_nr)
    } else {
      snk_fig(fig())
    }
  })  
  
  #-supporting sankey=============================================================
  
  
}
