
#server for D3WSOCRatings

D3_2022 <- read_csv("D3_2022_massey.csv")
D3_2023 <- read_csv("D3_2023.csv")
team_details <- read_csv("D3_teams_Massey.csv")
team_details[nrow(team_details) + 1, ] = list("", "", "", "all", "all")
regions = c(
  "Region I",
  "Region II",
  "Region III",
  "Region IV",
  "Region V",
  "Region VI",
  "Region VII",
  "Region VIII",
  "Region IX",
  "Region X",
  "all"
)
conferences = sort(unique(team_details$Conference))

server <- function(input, output) {
  D3_year <- reactive(if (input$Year == "2022") {
    D3_2022
  } else {
    D3_2023
  })
  
  # SCHEDULE FUNCTION
  schedule_function <-
    function(schedule = D3_year(),
             team_list = team_details,
             conf = "all",
             reg = "all",
             league_only = TRUE) {
      if (conf != "all") {
        team_list <- team_list %>%
          filter(Conference == conf)
      }
      
      if (reg != "all") {
        team_list <- team_list %>%
          filter(Region == reg)
      }
      
      is_team_in_league_AWAY <-
        ifelse(schedule[['away_team']] %in% team_list$School, TRUE, FALSE)
      is_team_in_league_HOME <-
        ifelse(schedule[['home_team']] %in% team_list$School, TRUE, FALSE)
      df_Away <- as.data.frame(is_team_in_league_AWAY)
      df_Home <- as.data.frame(is_team_in_league_HOME)
      df_HOME_AWAY <- bind_cols(df_Away, df_Home, schedule)
      
      # schedule of teams in league since league_only is true
      if (league_only) {
        games <- df_HOME_AWAY %>%
          filter(is_team_in_league_AWAY == TRUE &
                   is_team_in_league_HOME == TRUE) %>%
          select(away_team, away_score, home_team, home_score, date)
        
        
      } else {
        # want out of conference team schedule
        
        games <- df_HOME_AWAY %>%
          filter(is_team_in_league_AWAY == TRUE |
                   is_team_in_league_AWAY == TRUE) %>%
          select(away_team, away_score, home_team, home_score, date)
      }
      
      return (games)
    }
  
  
  # STANDINGS FUNCTION
  standings_function <-
    function(schedule = D3_year(),
             team_list = team_details,
             conf = "all",
             reg = "all",
             league_only = TRUE) {
      # if the user picks a specific conference rather than default of all conferences
      if (conf != "all") {
        team_list <- team_list %>%
          filter(Conference == conf)
      }
      
      # if the user picks a specific region rather than default of all regions
      if (reg != "all") {
        team_list <- team_list %>%
          filter(Region == reg)
      }
      
      is_team_in_league_AWAY <-
        ifelse(schedule[['away_team']] %in% team_list$School, TRUE, FALSE)
      is_team_in_league_HOME <-
        ifelse(schedule[['home_team']] %in% team_list$School, TRUE, FALSE)
      df_Away <- as.data.frame(is_team_in_league_AWAY)
      df_Home <- as.data.frame(is_team_in_league_HOME)
      df_HOME_AWAY <- bind_cols(df_Away, df_Home, schedule)
      
      # schedule of teams in league since league_only is true
      if (league_only) {
        games <- df_HOME_AWAY %>%
          filter(is_team_in_league_AWAY == TRUE &
                   is_team_in_league_HOME == TRUE) %>%
          select(home_team, home_score, away_team, away_score, date)
        
        
      } else {
        # want out of conference team schedule
        
        games <- df_HOME_AWAY %>%
          filter(is_team_in_league_AWAY == TRUE |
                   is_team_in_league_AWAY == TRUE) %>%
          select(home_team, home_score, away_team, away_score, date)
      }
      
      
      # to get the home goals a teams scores and concedes
      home_goals <- games %>%
        group_by(home_team) %>%
        summarize(
          home_goals_for = sum(home_score, na.rm = TRUE),
          home_goals_against = sum(away_score, na.rm = TRUE)
        ) %>%
        rename("School" = home_team)
      
      standings <- left_join(team_list, home_goals, by = "School")
      
      # to get the away goals a team scores and concedes
      away_goals <- games %>%
        group_by(away_team) %>%
        summarize(
          away_goals_for = sum(away_score, na.rm = TRUE),
          away_goals_against = sum(home_score, na.rm = TRUE)
        ) %>%
        rename("School" = away_team)
      
      standings <- left_join(standings, away_goals, by = "School")
      
      # to get total goals and goal differential for each team
      standings <- standings %>%
        mutate(
          goals_scored = (home_goals_for + away_goals_for),
          goals_conceded = (home_goals_against + away_goals_against),
          goal_differential = goals_scored - goals_conceded
        )
      
      # to get a team's home points
      home_points <- games %>%
        group_by(home_team) %>%
        mutate(points = ifelse(
          home_score > away_score,
          3,
          ifelse(home_score == away_score, 1, 0)
        )) %>%
        summarize(
          home_points = sum(points, na.rm = TRUE),
          home_wins = sum(points == 3, na.rm = TRUE),
          home_ties = sum(points == 1, na.rm = TRUE),
          home_losses = sum(points == 0, na.rm = TRUE)
        ) %>%
        rename("School" = home_team)
      
      standings <- left_join(standings, home_points, by = "School")
      
      # to get a team's away points
      away_points <- games %>%
        group_by(away_team) %>%
        mutate(points = ifelse(
          away_score > home_score,
          3,
          ifelse(home_score == away_score, 1, 0)
        )) %>%
        summarize(
          away_points = sum(points, na.rm = TRUE),
          away_wins = sum(points == 3, na.rm = TRUE),
          away_ties = sum(points == 1, na.rm = TRUE),
          away_losses = sum(points == 0, na.rm = TRUE)
        ) %>%
        rename("School" = away_team)
      
      standings <- left_join(standings, away_points, by = "School")
      
      # to get total points
      standings <- standings %>%
        mutate(
          points = home_points + away_points,
          wins = home_wins + away_wins,
          ties = home_ties + away_ties,
          losses = home_losses + away_losses,
          games_played = wins + ties + losses,
          ppg = round((points / games_played), 2),
          goal_differential_pg = round((goal_differential / games_played), 2),
          win_percentage = round((wins + 0.5 * ties) / games_played, 2)
        )
      
      # to format the standings
      standings <- standings %>%
        arrange(desc(points), desc(goal_differential)) %>%
        select(
          School,
          points,
          goal_differential,
          goals_scored,
          goals_conceded,
          wins,
          losses,
          ties,
          games_played,
          ppg,
          goal_differential_pg,
          win_percentage
        ) %>%
        rename(
          Points = points,
          'Goal Differential' = goal_differential,
          'Goals Scored' = goals_scored,
          'Goals Conceded' = goals_conceded,
          Wins = wins,
          Losses = losses,
          Ties = ties,
          Games = games_played,
          PPG = ppg,
          'Goal Differential Per Game' = goal_differential_pg,
          'Win Percentage' = win_percentage
        )
      
      return(standings)
    }
  
  # SECOND FUNCTION FOR STANDINGS
  # function that takes in schedule of games with no extraneous teams: use
  # this function for rpi_wp and rpi_ppg
  standings2 <- function(games) {
    # to get the home goals a teams scores and concedes
    home_goals <- games %>%
      group_by(home_team) %>%
      summarize(
        home_goals_for = sum(home_score, na.rm = TRUE),
        home_goals_against = sum(away_score, na.rm = TRUE)
      ) %>%
      rename("School" = home_team)
    
    # to get the away goals a team scores and concedes
    away_goals <- games %>%
      group_by(away_team) %>%
      summarize(
        away_goals_for = sum(away_score, na.rm = TRUE),
        away_goals_against = sum(home_score, na.rm = TRUE)
      ) %>%
      rename("School" = away_team)
    
    standings <- left_join(home_goals, away_goals, by = "School")
    
    
    # to get total goals and goal differential for each team
    standings <- standings %>%
      mutate(
        goals_scored = (home_goals_for + away_goals_for),
        goals_conceded = (home_goals_against + away_goals_against),
        goal_differential = goals_scored - goals_conceded
      )
    
    
    # to get a team's home points
    home_points <- games %>%
      group_by(home_team) %>%
      mutate(points = ifelse(
        home_score > away_score,
        3,
        ifelse(home_score == away_score, 1, 0)
      )) %>%
      summarize(
        home_points = sum(points, na.rm = TRUE),
        home_wins = sum(points == 3, na.rm = TRUE),
        home_ties = sum(points == 1, na.rm = TRUE),
        home_losses = sum(points == 0, na.rm = TRUE)
      ) %>%
      rename("School" = home_team)
    
    standings <- left_join(standings, home_points, by = "School")
    
    # to get a team's away points
    away_points <- games %>%
      group_by(away_team) %>%
      mutate(points = ifelse(
        away_score > home_score,
        3,
        ifelse(home_score == away_score, 1, 0)
      )) %>%
      summarize(
        away_points = sum(points, na.rm = TRUE),
        away_wins = sum(points == 3, na.rm = TRUE),
        away_ties = sum(points == 1, na.rm = TRUE),
        away_losses = sum(points == 0, na.rm = TRUE)
      ) %>%
      rename("School" = away_team)
    
    standings <- left_join(standings, away_points, by = "School")
    
    # to get total points
    standings <- standings %>%
      mutate(
        points = home_points + away_points,
        wins = home_wins + away_wins,
        ties = home_ties + away_ties,
        losses = home_losses + away_losses,
        record = paste(wins, losses, ties, sep = "-"),
        games_played = wins + ties + losses,
        ppg = (points / games_played),
        goal_differential_pg = (goal_differential / games_played),
        win_percentage = ((wins + 0.5 * ties) / games_played)
      )
    
    # to format the standings
    standings <- standings %>%
      arrange(desc(points), desc(goal_differential)) %>%
      select(
        School,
        points,
        goal_differential,
        goals_scored,
        goals_conceded,
        wins,
        losses,
        ties,
        record,
        games_played,
        ppg,
        goal_differential_pg,
        win_percentage
      ) %>%
      rename(
        Points = points,
        'Goal Differential' = goal_differential,
        'Goals Scored' = goals_scored,
        'Goals Conceded' = goals_conceded,
        Wins = wins,
        Losses = losses,
        Ties = ties,
        'Record (W-L-T)' = record,
        Games = games_played,
        PPG = ppg,
        'Goal Differential Per Game' = goal_differential_pg,
        'Win Percentage' = win_percentage
      )
    
    return(standings)
  }
  
  
  # RPI_PPG FUNCTION
  rpi_function_ppg <-
    function(games,
             w1 = (0.25),
             w2 = (0.5),
             w3 = (0.25)) {
      # w1, w2, w3 are weights
      
      stand <- standings2(games)
      
      games <- games %>%
        rename(
          team = home_team,
          opponent = away_team,
          team_score = home_score,
          opp_score = away_score
        )
      
      games_2 <- games %>%
        rename(
          team = opponent,
          opponent = team,
          team_score = opp_score,
          opp_score = team_score
        )
      
      double_table <- bind_rows(games, games_2)
      
      ppg1 <-
        left_join(double_table, stand, by = c("opponent" = "School")) %>%
        select(team, PPG, 'Win Percentage') %>%
        rename(opp_ppg = PPG)
      
      opp_ppg <- ppg1 %>%
        group_by(team) %>%
        mutate(avg_opp_ppg = mean(opp_ppg)) %>%
        distinct(team, .keep_all = T) %>%
        select(team, avg_opp_ppg)
      
      new_teams_ppg <-
        left_join(stand, opp_ppg, by = c("School" = "team")) %>%
        rename("team" = "School")
      
      ppg2 <-
        left_join(double_table, new_teams_ppg, by = c("opponent" =
                                                        "team")) %>%
        select(-c(date, opp_score, team_score)) %>%
        rename(opp_ppg = PPG)
      
      new_opp_ppg <- ppg2 %>%
        group_by(team) %>%
        mutate(avg_opp_opp_ppg = mean(avg_opp_ppg)) %>%
        distinct(team, .keep_all = T) %>%
        select(team, avg_opp_opp_ppg)
      
      full_teams_ppg <-
        left_join(new_teams_ppg, new_opp_ppg, by = "team") %>% # to add opponents opponents points per game
        mutate(RPI = (PPG * w1 + avg_opp_ppg * w2 + avg_opp_opp_ppg * w3)) %>%
        arrange(desc(RPI)) %>%
        select(team, RPI, PPG, avg_opp_ppg, avg_opp_opp_ppg) %>%
        rename(
          'Team' = team,
          'RPI' = RPI,
          'PPG' = PPG,
          'OPPG' = avg_opp_ppg,
          'OOPPG' = avg_opp_opp_ppg
        ) %>%
        mutate_if(is.numeric, round, digits = 2)
      
      full_teams_ppg$Rank = 1:nrow(full_teams_ppg)
      
      full_teams_ppg <- full_teams_ppg %>%
        relocate(Rank)
      
      return(full_teams_ppg)
    }
  
  
  # RPI_WP FUNCTION
  rpi_function_wp <-
    function(games,
             w1 = (0.25),
             w2 = (0.5),
             w3 = (0.25)) {
      stand <- standings2(games)
      
      games <- games %>%
        rename(
          team = home_team,
          opponent = away_team,
          team_score = home_score,
          opp_score = away_score
        )
      
      games_2 <- games %>%
        rename(
          team = opponent,
          opponent = team,
          team_score = opp_score,
          opp_score = team_score
        )
      
      double_table <- bind_rows(games, games_2)
      
      wp1 <-
        left_join(double_table, stand, by = c("opponent" = "School")) %>%
        select(team, PPG, 'Win Percentage') %>%
        rename(opp_wp = 'Win Percentage')
      
      opp_wp <- wp1 %>%
        group_by(team) %>%
        mutate(avg_opp_wp = mean(opp_wp)) %>%
        distinct(team, .keep_all = T) %>%
        select(team, avg_opp_wp)
      
      new_teams_wp <-
        left_join(stand, opp_wp, by = c("School" = "team")) %>%
        rename("team" = "School")
      
      wp2 <- left_join(double_table, new_teams_wp, by = c("opponent" =
                                                            "team")) %>%
        select(-c(date, opp_score, team_score)) %>%
        rename(opp_wp = 'Win Percentage')
      
      new_opp_wp <- wp2 %>%
        group_by(team) %>%
        mutate(avg_opp_opp_wp = mean(avg_opp_wp)) %>%
        distinct(team, .keep_all = T) %>%
        select(team, avg_opp_opp_wp)
      
      full_teams_wp <-
        left_join(new_teams_wp, new_opp_wp, by = "team") %>% # to add opponents opponents WP
        rename(WP = 'Win Percentage') %>%
        mutate(RPI = (WP * w1 + avg_opp_wp * w2 + avg_opp_opp_wp * w3)) %>%
        arrange(desc(RPI)) %>%
        select(team, RPI, WP, avg_opp_wp, avg_opp_opp_wp) %>%
        rename(
          'Team' = team,
          'RPI' = RPI,
          'WP' = WP,
          'OWP' = avg_opp_wp,
          'OOWP' = avg_opp_opp_wp
        ) %>%
        mutate_if(is.numeric, round, digits = 2)
      
      full_teams_wp$Rank = 1:nrow(full_teams_wp)
      
      full_teams_wp <- full_teams_wp %>%
        relocate(Rank)
      
      
      return(full_teams_wp)
    }
  
  # DOUBLE TABLE FUNCTION
  doubleTableFunc <- function(games) {
    games <- games %>%
      rename(
        team = home_team,
        opponent = away_team,
        team_score = home_score,
        opp_score = away_score
      ) %>%
      filter(!is.na(team_score & opp_score))
    games2 <- games %>% # new data table flipping the columns
      rename(
        team = opponent,
        opponent = team,
        team_score = opp_score,
        opp_score = team_score
      )
    double_table <- bind_rows(games, games2) # paste them together
    double_table$Location = NA # add location variable
    double_table$Location[0:length(double_table$date) / 2] = 1
    double_table[is.na(double_table)] = -1
    
    return(double_table)
  }
  
  # BESSEL FUNCTION for calculating expected points
  probs <- function(lambda1, lambda2) {
    prob_tie <-
      besselI(2 * sqrt(lambda1 * lambda2), 0) * exp(-(lambda1 + lambda2))
    
    total <- 0
    for (i in 1:20) {
      prob_win_iteration <-
        besselI(2 * sqrt(lambda1 * lambda2), i) * (lambda1 / lambda2) ^ (i / 2) *
        exp(-(lambda1 + lambda2))
      
      total = total + prob_win_iteration
      i = i + 1
      
    }
    
    prob_win <- total
    
    prob_loss <- 1 - (prob_tie) - (prob_win)
    
    return(list(prob_win, prob_tie, prob_loss))
  }
  
  
  
  # POISSON FUNCTION
  poisfunction <- function(schedule) {
    # based on a team's offensive and defensive ratings
    #team_score is the home team score
    model <-
      glm(
        team_score ~ team + opponent + Location,
        data = doubleTableFunc(schedule),
        family = poisson
      ) # to build the poisson model
    original <-
      data.frame(x = model$coefficients,
                 rating = exp(model$coefficients)) # to extract the coefficients
    original$team <- rownames(original)
    row.names(original) <- NULL
    original <- original %>%
      select(team, x, rating)
    
    
    defAVG <- (original %>%
                 filter(str_detect(team, "opponent") == TRUE) %>%
                 summarise(sum(x) / (n() + 1)))[1, 1]
    offAVG <- (original %>%
                 filter(str_detect(team, "team") == TRUE) %>%
                 summarise(sum(x) / (n() + 1)))[1, 1]
    intercept <- (original$x)[1]
    homeAdj <- original$rating[length(original$rating)]
    adjusted <-
      data.frame(team = unique(doubleTableFunc(schedule)$team))
    adjusted <- data.frame(team = adjusted[order(adjusted$team),])
    adjusted$Off = append(0, original$x[2:(length(original$x) / 2)])
    adjusted$Def = append(0, original$x[(length(original$x) / 2 + 1):(length(original$x) -
                                                                        1)])
    adjusted$OffAdj = adjusted$Off + defAVG + intercept
    adjusted$DefAdj = adjusted$Def + offAVG + intercept
    adjusted$expOff = exp(adjusted$Off + defAVG + intercept)
    adjusted$expDef = exp(adjusted$Def + offAVG + intercept)
    
    # bump scoring rate to 0.05
    adjusted$expOff[adjusted$expOff < 0.05] <- 0.05
    
    # bump defensive rate to 0.05
    adjusted$expDef[adjusted$expDef < 0.05] <- 0.05
    
    #average offensive
    avgPoisOffCoef <- mean(adjusted$OffAdj)
    # average defensive
    avgPoisDefCoef <- mean(adjusted$DefAdj)
    avgPoisCoef <-
      exp((avgPoisDefCoef + avgPoisOffCoef) / 2) # avg coefficient
    scoreRate <-
      mean(doubleTableFunc(schedule)$team_score) # average scoring rate
    C = scoreRate / avgPoisCoef
    poissonOFFavg = mean(adjusted$expOff)
    poissonDEFavg = mean(adjusted$expDef)
    D = sqrt(C * (poissonDEFavg / poissonOFFavg))
    adjusted$newOffExp = adjusted$expOff * D
    adjusted$newDefExp = adjusted$expDef * (C / D)
    
    returnedTable <- adjusted %>%
      mutate(
        OffRating = newOffExp,
        DefRating = newDefExp,
        OverallRating = newOffExp / newDefExp
      ) %>%
      rename(Team = "team") %>%
      select(Team,
             OffRating,
             DefRating,
             OverallRating) %>%
      arrange(desc(OffRating))
    returnedTable$OffRank = 1:nrow(returnedTable)
    
    returnedTable <- returnedTable %>%
      arrange(DefRating)
    returnedTable$DefRank = 1:nrow(returnedTable)
    
    returnedTable <- returnedTable %>%
      arrange(desc(OverallRating)) %>%
      mutate_if(is.numeric, round, digits = 2) %>%
      select(Team,
             OverallRating,
             OffRating,
             OffRank,
             DefRating,
             DefRank)
    
    avgOffRating <- mean(returnedTable$OffRating)
    
    sum = rep(0, length(returnedTable$Team))
    
    for (i in 1:length(returnedTable$Team)) {
      for (j in 1:length(returnedTable$Team)) {
        lambda1 = (returnedTable$OffRating[i] * returnedTable$DefRating[j]) / (avgOffRating)
        
        lambda2 = (returnedTable$OffRating[j] * returnedTable$DefRating[i]) / (avgOffRating)
        
        x <- probs(lambda1, lambda2)
        sum[i] = sum[i] + (3 * x[[1]]) + x[[2]]
      }
    }
    
    expected_points <- sum / (length(returnedTable$Team))
    returnedTable$OverallRating <- round(expected_points, 2)
    
    returnedTable <- returnedTable %>%
      arrange(desc(OverallRating))
    
    returnedTable$Rank = 1:nrow(returnedTable)
    
    returnedTable <- returnedTable %>%
      select(Rank,
             Team,
             OverallRating,
             OffRating,
             OffRank,
             DefRating,
             DefRank)
    
    return(list(returnedTable, homeAdj, avgOffRating))
  }
  
  # ELO FUNCTION
  elo_rating_function <- function(schedule) {
    elo_ratings <-
      elo.run(
        score(home_score, away_score) ~ adjust(home_team, input$home_advantage) + away_team + k(input$k_value + abs(home_score - away_score) * 10),
        data = schedule
      )
    
    elo_object <- as.matrix(elo_ratings)
    
    elo_object <- elo_object %>%
      tail(n = 1) # select only the last row
    
    rownames(elo_object) <- NULL
    
    elo_object <- elo_object %>%
      t() # use the transpose function in R to switch columns and rows
    
    elo_df <- as.data.frame(elo_object) %>%
      cbind(rownames(elo_object), elo_object) %>%
      select(2:3) %>%
      rename(Team = "rownames(elo_object)", "Elo Rating" = "elo_object") %>%
      mutate_if(is.numeric, round, digits = 2) %>%
      arrange(desc(`Elo Rating`))
    
    rownames(elo_df) <- NULL
    
    elo_df$Rank = 1:nrow(elo_df)
    
    elo_df <- elo_df %>%
      relocate(Rank)
    
    return (elo_df)
  }
  
  
  
  # Output code to call functions
  output$ConfRegBox <- renderUI({
    if (input$ConfReg == "Conference") {
      selectInput("Conference",
                  "Choose a Conference",
                  choices = conferences,
                  selected = "LL")
    } else {
      selectInput("Region",
                  "Choose a Region",
                  choices = regions,
                  selected = "Region III")
    }
  })
  
  
  games <- reactive(if (input$ConfReg == "Conference") {
    schedule_function(conf = input$Conference)
  } else {
    schedule_function(reg = input$Region)
  })
  
  output$games <- renderDataTable(games())
  
  standings <- reactive(if (input$ConfReg == "Conference") {
    standings_function(conf = input$Conference)
  } else {
    standings_function(reg = input$Region)
  })
  
  output$standings <- renderDataTable(standings())
  
  rpi_ppg <- reactive(rpi_function_ppg(games()))
  
  output$rpi_ppg <- renderDataTable(rpi_ppg())
  
  rpi_wp <- reactive(rpi_function_wp(games()))
  
  output$rpi_wp <- renderDataTable(rpi_wp())
  
  poisson_x <- reactive(poisfunction(games()))
  
  output$poisson_table <- renderDataTable(poisson_x()[[1]])
  
  output$poisson_stats <- renderText(paste(
    "Mean Rating = ",
    round(poisson_x()[[2]], 3),
    "   Home Adjustment = ",
    round(poisson_x()[[3]], 3)
  ))
  
  elo_rating_1 <- reactive(elo_rating_function(games()))
  
  output$elo_rating <- renderDataTable(elo_rating_1())
  
  output$downloadgames <- downloadHandler(
    filename = "Games.csv",
    content = function(file) {
      write.csv(games(), file, row.names = FALSE)
    }
  )
  
  output$downloadstandings <- downloadHandler(
    filename = "Standings.csv",
    content = function(file) {
      write.csv(standings(), file, row.names = FALSE)
    }
  )
  
  output$downloadpoisson <- downloadHandler(
    filename = "Poisson.csv",
    content = function(file) {
      write.csv(poisson_x()[[1]], file, row.names = FALSE)
    }
  )
  
  output$downloadelo <- downloadHandler(
    filename = "Elo.csv",
    content = function(file) {
      write.csv(elo_rating_1(), file, row.names = FALSE)
    }
  )
  
  output$downloadrpi_ppg <- downloadHandler(
    filename = "RPI_PPG.csv",
    content = function(file) {
      write.csv(rpi_ppg(), file, row.names = FALSE)
    }
  )
  
  output$downloadrpi_wp <- downloadHandler(
    filename = "RPI_WP.csv",
    content = function(file) {
      write.csv(rpi_wp(), file, row.names = FALSE)
    }
  )
  
  
}