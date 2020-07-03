#Data miming application 
# Alunos: Bruno, Ulysses e Maria Julia 

{ 
  # Check if the packages that we need are installed
  want = c("tidyverse", "data.table", "readr", "stringr", "zoo", "plyr", "dplyr", "COVID19", "usethis", "tidyr")
  have = want %in% rownames(installed.packages())
  # Install the packages that we miss
  if ( any(!have) ) { install.packages( want[!have] ) }
  # Load the packages
  junk <- lapply(want, library, character.only = T)
  # Remove the objects we created
  rm(have, want, junk)
} # import packages

#{
usethis::use_git_config(user.name = "Maria Julia Garcia", # full name
                        user.email = "m.juliagarcia@gmail.com") # Semail
usethis::browse_github_token()

GITHUB_PAT= "cc37860c52da69313c3fa82c617603cce9f303d7"

#} # git information

{
  ## Import COVID cities database
  
  # Declare the download link 
  url = "https://data.brasil.io/dataset/covid19/caso.csv.gz"
  
  # Create temporary file
  tmp = tempfile()
  
  # Download the .gz file
  download.file(url,tmp)
  
  # Finish import process
  dcovid19 =   read_csv(gzfile(tmp),
                        col_types = cols(date = col_date(format = "%Y-%m-%d")), 
                        locale = locale(decimal_mark = ",", grouping_mark = ".", 
                                        encoding = "UTF-8"))
  
  dcovid19 <- as.data.frame(dcovid19)
  
  # Create dcityam database   
  dcityam = dcovid19 %>% filter(state == "AM" & place_type == "city" & date > "2020-03-13") %>%
    mutate(select = case_when(city_ibge_code == 1300029 ~ "Alvarães",
                              city_ibge_code == 1300060 ~ "Amaturá",
                              city_ibge_code == 1300086 ~ "Anamã",
                              city_ibge_code == 1300607 ~ "Benjamin Constant",
                              city_ibge_code == 1300144 ~ "Apuí",
                              city_ibge_code == 1300201 ~ "Atalaia do Norte",
                              city_ibge_code == 1300680 ~ "Boa Vista do Ramos",
                              city_ibge_code == 1300706 ~ "Boca do Acre",
                              city_ibge_code == 1301159 ~ "Careiro da Várzea",
                              city_ibge_code == 1301407 ~ "Eirunepé",
                              city_ibge_code == 1301605 ~ "Fonte Boa",
                              city_ibge_code == 1301308 ~ "Codajás",
                              city_ibge_code == 1301654 ~ "Guajará",
                              city_ibge_code == 1301704 ~ "Humaitá",
                              city_ibge_code == 1302108 ~ "Japurá",
                              city_ibge_code == 1302207 ~ "Juruá",
                              city_ibge_code == 1302306 ~ "Jutaí",
                              city_ibge_code == 1302405 ~ "Lábrea",
                              city_ibge_code == 1302702 ~ "Manicoré",
                              city_ibge_code == 1302801 ~ "Maraã",
                              city_ibge_code == 1302900 ~ "Maués",
                              city_ibge_code == 1303007 ~ "Nhamundá",
                              city_ibge_code == 1303106 ~ "Nova Olinda do Norte",
                              city_ibge_code == 1303205 ~ "Novo Airão",
                              city_ibge_code == 1303304 ~ "Novo Aripuanã",
                              city_ibge_code == 1303536 ~ "Presidente Figueiredo",
                              city_ibge_code == 1303569 ~ "Rio Preto da Eva",
                              city_ibge_code == 1303601 ~ "Santa Isabel do Rio Negro",
                              city_ibge_code == 1303700 ~ "Santo Antônio do Içá",
                              city_ibge_code == 1303809 ~ "São Gabriel da Cachoeira",
                              city_ibge_code == 1303957 ~ "São Sebastião do Uatumã",
                              city_ibge_code == 1303908 ~ "São Paulo de Olivença",
                              city_ibge_code == 1304104 ~ "Tapauá",
                              city_ibge_code == 1304203 ~ "Tefé",
                              city_ibge_code == 1304302 ~ "Urucará",
                              city_ibge_code == 1300102 ~ "Anori",
                              city_ibge_code == 1300300 ~ "Autazes",
                              city_ibge_code == 1300508 ~ "Barreirinha",
                              city_ibge_code == 1300409 ~ "Barcelos",
                              city_ibge_code == 1300631 ~ "Beruri",
                              city_ibge_code == 1300805 ~ "Borba",
                              city_ibge_code == 1300839 ~ "Caapiranga",
                              city_ibge_code == 1300904 ~ "Canutama",
                              city_ibge_code == 1301001 ~ "Carauari",
                              city_ibge_code == 1301100 ~ "Careiro",
                              city_ibge_code == 1301209 ~ "Coari",
                              city_ibge_code == 1301803 ~ "Ipixuna",
                              city_ibge_code == 1301852 ~ "Iranduba",
                              city_ibge_code == 1301902 ~ "Itacoatiara",
                              city_ibge_code == 1301951 ~ "Itamarati",
                              city_ibge_code == 1302009 ~ "Itapiranga",
                              city_ibge_code == 1302504 ~ "Manacapuru",
                              city_ibge_code == 1302554 ~ "Manaquiri",
                              city_ibge_code == 1302603 ~ "Manaus",
                              city_ibge_code == 1303403 ~ "Parintins",
                              city_ibge_code == 1303502 ~ "Pauini",
                              city_ibge_code == 1304005 ~ "Silves",
                              city_ibge_code == 1304062 ~ "Tabatinga",
                              city_ibge_code == 1304260 ~ "Uarini",
                              city_ibge_code == 1304237 ~ "Tonantins",
                              city_ibge_code == 1304401 ~ "Urucurituba")) %>%
    arrange(desc(date)) %>%
    group_by(date, select) %>%
    dplyr::summarize(confirmed = sum(confirmed),
                     deaths = sum(deaths),
                     population = sum(estimated_population_2019)) 
  
  # Include a time lag variables
  setDT(dcityam)[, deaths_1 := shift(deaths, fill=0), by = select]
  setDT(dcityam)[, confirmed_1 := shift(confirmed, fill=0), by = select]
  
  # Organize dcityam database
  dcityam = dcityam %>% mutate(deaths_new = deaths - deaths_1,
                               confirmed_new = confirmed - confirmed_1) %>%
    ungroup() %>% 
    select(date, select, confirmed, confirmed_new, deaths, deaths_new,population) %>%
    arrange(desc(date)) #essa agrega infos gerais
  
  dcityam1 = dcityam %>% tidyr::gather(key = "variable", value = "occurrences", deaths, deaths_new, confirmed, confirmed_new) %>%
    mutate(occurrences = as.numeric(occurrences)) %>%
    mutate(occurrences = case_when(occurrences < 0 ~ 0,
                                   TRUE ~ occurrences)) #essa é só por ocorrencia de mortes
  
  # Drop unnecessary databases
  #remove(list = c("dcovid19", "tmp", "url"))  
  
} # database by city


####
#possiveis usos
####

#dcityam1 = dcityam %>% 
# mutate(drop = case_when(date == max(date) & confirmed_new <= 0 ~ TRUE,
#                        TRUE ~ FALSE)) %>% 