
get_countries_by_continent <- function(continent) {
  asia <- c(
    "Israel",
    "United Arab Emirates",
    "Singapore",
    "Thailand",
    "Taiwan Province of China",
    "Qatar",
    "Saudi Arabia",
    "Kuwait",
    "Bahrain",
    "Malaysia",
    "Uzbekistan",
    "Japan",
    "South Korea",
    "Turkmenistan",
    "Kazakhstan",
    "Turkey",
    "Hong Kong S.A.R., China",
    "Philippines",
    "Jordan",
    "China",
    "Pakistan",
    "Indonesia",
    "Azerbaijan",
    "Lebanon",
    "Vietnam",
    "Tajikistan",
    "Bhutan",
    "Kyrgyzstan",
    "Nepal",
    "Mongolia",
    "Palestinian Territories",
    "Iran",
    "Bangladesh",
    "Myanmar",
    "Iraq",
    "Sri Lanka",
    "Armenia",
    "India",
    "Georgia",
    "Cambodia",
    "Afghanistan",
    "Yemen",
    "Syria"
  )
  
  europe <- c(
    "Norway",
    "Denmark",
    "Iceland",
    "Switzerland",
    "Finland",
    "Netherlands",
    "Sweden",
    "Austria",
    "Ireland",
    "Germany",
    "Belgium",
    "Luxembourg",
    "United Kingdom",
    "Czech Republic",
    "Malta",
    "France",
    "Spain",
    "Slovakia",
    "Poland",
    "Italy",
    "Russia",
    "Lithuania",
    "Latvia",
    "Moldova",
    "Romania",
    "Slovenia",
    "North Cyprus",
    "Cyprus",
    "Estonia",
    "Belarus",
    "Serbia",
    "Hungary",
    "Croatia",
    "Kosovo",
    "Montenegro",
    "Greece",
    "Portugal",
    "Bosnia and Herzegovina",
    "Macedonia",
    "Bulgaria",
    "Albania",
    "Ukraine"
  )
  
  north_america <- c(
    "Canada",
    "Costa Rica",
    "United States",
    "Mexico",
    "Panama",
    "Trinidad and Tobago",
    "El Salvador",
    "Belize",
    "Guatemala",
    "Jamaica",
    "Nicaragua",
    "Dominican Republic",
    "Honduras",
    "Haiti"
  )
  
  south_america <- c(
    "Chile",
    "Brazil",
    "Argentina",
    "Uruguay",
    "Colombia",
    "Ecuador",
    "Bolivia",
    "Peru",
    "Paraguay",
    "Venezuela"
  )
  
  australia <- c("New Zealand", "Australia")
  
  if(continent == "asia") {
    return(asia)
  } else if (continent == "europe") {
    return(europe)
  } else if (continent == "north america") {
    return(north_america)
  } else if (continent == "south america") {
    return(south_america)
  } else if (continent == "australia") {
    return(australia)
  } 
}

get_allendale_students_data <- function() {
  allendale_students <- readr::read_csv("data/allendale-students.csv")
  
  data <- allendale_students %>%
    filter(!row_number() %in% c(11))
  
  return(data)
}

get_student_exam_scores_data <-
  function(data = "data/exam_scores.csv") {
    exam_scores <-
      as_tibble(read.csv(data, stringsAsFactors = T))
    
    exam_scores <-
      exam_scores %>% dplyr::rename(
        "ethnicity" = "race.ethnicity",
        "parental_education" = "parental.level.of.education",
        "test_preparation_course" = "test.preparation.course",
        "math_score" = "math.score",
        "reading_score" = "reading.score",
        "writing_score" = "writing.score"
      )
    
    exam_scores <- exam_scores %>%
      rowwise() %>%
      mutate(average_score = mean(reading_score, writing_score, math_score))
    
    exam_scores$good_student <-
      ifelse(exam_scores$average_score > mean(exam_scores$average_score),
             1,
             0)
    
    exam_scores$is_male <-
      ifelse(exam_scores$gender == "male", 1, 0)
    exam_scores$is_female <-
      ifelse(exam_scores$gender == "female", 1, 0)
    
    exam_scores$ethnicity_A <-
      ifelse(exam_scores$ethnicity == "group A", 1, 0)
    exam_scores$ethnicity_B <-
      ifelse(exam_scores$ethnicity == "group B", 1, 0)
    exam_scores$ethnicity_C <-
      ifelse(exam_scores$ethnicity == "group C", 1, 0)
    exam_scores$ethnicity_D <-
      ifelse(exam_scores$ethnicity == "group D", 1, 0)
    exam_scores$ethnicity_E <-
      ifelse(exam_scores$ethnicity == "group E", 1, 0)
    
    exam_scores$parents_edu_associate <-
      ifelse(exam_scores$parental_education
             == "associate's degree",
             1,
             0)
    exam_scores$parents_edu_bachelor <-
      ifelse(exam_scores$parental_education
             == "bachelor's degree",
             1,
             0)
    exam_scores$parents_edu_hschool <-
      ifelse(
        exam_scores$parental_education
        == "high school" |
          exam_scores$parental_education ==
          "some high school",
        1,
        0
      )
    exam_scores$parents_edu_masters <-
      ifelse(exam_scores$parental_education
             == "master's degree",
             1,
             0)
    exam_scores$parents_edu_college <-
      ifelse(exam_scores$parental_education
             == "some college", 1, 0)
    
    exam_scores$standard_lunch <-
      ifelse(exam_scores$lunch == "standard", 1, 0)
    exam_scores$subsidised_lunch <-
      ifelse(exam_scores$lunch == "free/reduced", 1, 0)
    
    exam_scores$test_prep_complete <-
      ifelse(exam_scores$test_preparation_course == "completed"
             , 1, 0)
    
    data <- subset(
      exam_scores,
      select = -c(
        gender,
        ethnicity,
        parental_education,
        lunch,
        test_preparation_course
      )
    )
    
    data <- data[, c(6:20, 1:5)]
    
    return(data)
  }

get_happiness_data <- function() {
  Happiness <- read.csv("data/happiness_report_2022.csv")
  colnames (Happiness) <-
    c(
      "Happiness.Rank",
      "Country",
      "Happiness.Score",
      "Whisker.High",
      "Whisker.Low",
      "Dystopia.Residual",
      "Economy",
      "Family",
      "Life.Expectancy",
      "Freedom",
      "Generosity",
      "Trust"
    )
  Happiness <- Happiness[, -c(4, 5)]
  Happiness <-
    Happiness %>% select(
      Country,
      Happiness.Rank,
      Happiness.Score,
      Economy,
      Family,
      Life.Expectancy,
      Freedom,
      Generosity,
      Trust,
      Dystopia.Residual
    )
  
  Happiness$Continent <- NA
  
  Happiness$Continent[which(
    Happiness$Country %in% c(
      "Israel",
      "United Arab Emirates",
      "Singapore",
      "Thailand",
      "Taiwan Province of China",
      "Qatar",
      "Saudi Arabia",
      "Kuwait",
      "Bahrain",
      "Malaysia",
      "Uzbekistan",
      "Japan",
      "South Korea",
      "Turkmenistan",
      "Kazakhstan",
      "Turkey",
      "Hong Kong S.A.R., China",
      "Philippines",
      "Jordan",
      "China",
      "Pakistan",
      "Indonesia",
      "Azerbaijan",
      "Lebanon",
      "Vietnam",
      "Tajikistan",
      "Bhutan",
      "Kyrgyzstan",
      "Nepal",
      "Mongolia",
      "Palestinian Territories",
      "Iran",
      "Bangladesh",
      "Myanmar",
      "Iraq",
      "Sri Lanka",
      "Armenia",
      "India",
      "Georgia",
      "Cambodia",
      "Afghanistan",
      "Yemen",
      "Syria"
    )
  )] <- "Asia"
  
  Happiness$Continent[which(
    Happiness$Country %in% c(
      "Norway",
      "Denmark",
      "Iceland",
      "Switzerland",
      "Finland",
      "Netherlands",
      "Sweden",
      "Austria",
      "Ireland",
      "Germany",
      "Belgium",
      "Luxembourg",
      "United Kingdom",
      "Czech Republic",
      "Malta",
      "France",
      "Spain",
      "Slovakia",
      "Poland",
      "Italy",
      "Russia",
      "Lithuania",
      "Latvia",
      "Moldova",
      "Romania",
      "Slovenia",
      "North Cyprus",
      "Cyprus",
      "Estonia",
      "Belarus",
      "Serbia",
      "Hungary",
      "Croatia",
      "Kosovo",
      "Montenegro",
      "Greece",
      "Portugal",
      "Bosnia and Herzegovina",
      "Macedonia",
      "Bulgaria",
      "Albania",
      "Ukraine"
    )
  )] <- "Europe"
  Happiness$Continent[which(
    Happiness$Country %in% c(
      "Canada",
      "Costa Rica",
      "United States",
      "Mexico",
      "Panama",
      "Trinidad and Tobago",
      "El Salvador",
      "Belize",
      "Guatemala",
      "Jamaica",
      "Nicaragua",
      "Dominican Republic",
      "Honduras",
      "Haiti"
    )
  )] <- "North America"
  Happiness$Continent[which(
    Happiness$Country %in% c(
      "Chile",
      "Brazil",
      "Argentina",
      "Uruguay",
      "Colombia",
      "Ecuador",
      "Bolivia",
      "Peru",
      "Paraguay",
      "Venezuela"
    )
  )] <- "South America"
  Happiness$Continent[which(Happiness$Country %in% c("New Zealand", "Australia"))] <-
    "Australia"
  Happiness$Continent[which(is.na(Happiness$Continent))] <- "Africa"
  
  # Moving the continent column's position in the dataset to the second column
  
  Happiness <-
    Happiness %>% select(Country, Continent, everything())
  
  # Changing Continent column to factor
  
  Happiness$Continent <- as.factor(Happiness$Continent)
  Happiness$Country <- as.factor(Happiness$Country)
  Happiness <- na.omit(Happiness)
  return(Happiness)
}