### importation des données
data_description <- read.csv("~/Desktop/tasks/pygame-python/Montpellier/description-based/data_description_2.csv")

#virer les participants qui ont moins de 18 ans
unique(data_description[data_description$age < 18,]$participant)
#"xfry0a3g" "2nl33b9e"

data_description <- subset(data_description, data_description$participant != "xfry0a3g")
data_description <- subset(data_description, data_description$participant != "2nl33b9e")

# ajouter des numéro pour chaque participants 
base <- data_description[, c(1:13,102,103,106:226)]
base$id <- rep(1:54, each = 400)
base$id <- as.numeric(base$id)

##### données juste de la 3eme tache
data_fourchoices <- base
data_fourchoices$button_name <- as.factor(data_fourchoices$button_name)


########################## informations participants #############################

info_participant <- unique(base[,c(2,3,13:137)])
# et là on a tout direct

# age mean 24 +/- 8 
min(info_participant$age)
mean(info_participant$age)
sd(info_participant$age)
median(info_participant$age) # 21

# gender
table(info_participant$gender)# 31 garçons (1) ; 23 filles (0)

# status marital 
table(info_participant$marital_status)
# 48 = 0 célibataire ; 2 = 2 divorcé ; 3 = 1 marié ; 1 = 3 divorcé

# nationalité 
table(info_participant$nationality)
# 34 france (75); 

# socioprofessional_group
table(info_participant$socioprofessional_group)
# 69 étudiants (code 0),3 cadres (code 3), 2 artisans (code 2), 2 employé (code 5), 4 autres (code 8)

# dernier diplome obtenus
table(info_participant$diplome)
# 40 bac ; 22 licence ; 15 master ; 1 doctorat
# moyenne en moyenne dernier diplome obtenu licence

# discipline
table(info_participant$discipline)
# 14 SHS (code0); 39 eco gestion (code4) ; 8 bio (code2) ; 5 art (code3) ; 2 education (code5)

#### loss aversion et risque aversion 

# loss aversion 
info_participant$loss_aversion

# risk aversion = biswanger
info_participant$biswanger

##### questionnaire standardisés
# Big Five
info_participant$Extraversion
info_participant$Agreeableness
info_participant$Conscientiousness
info_participant$Neuroticism
info_participant$Openness

### BIS-11
# https://www.sralab.org/rehabilitation-measures/barratt-impulsiveness-scale#healthy-adults
#SI BESION inversion de l'ordre des valeurs pour les questions qui en ont besoin
mapping <- c(4,3,2,1)
info_participant$biseleven_1 <- mapping[info_participant$biseleven_1]
info_participant$biseleven_7 <- mapping[info_participant$biseleven_7]
info_participant$biseleven_8 <- mapping[info_participant$biseleven_8]
info_participant$biseleven_9 <- mapping[info_participant$biseleven_9]
info_participant$biseleven_10 <- mapping[info_participant$biseleven_10]
info_participant$biseleven_12 <- mapping[info_participant$biseleven_12]
info_participant$biseleven_13 <- mapping[info_participant$biseleven_13]
info_participant$biseleven_15 <- mapping[info_participant$biseleven_15]
info_participant$biseleven_20 <- mapping[info_participant$biseleven_20]
info_participant$biseleven_29 <- mapping[info_participant$biseleven_29]
info_participant$biseleven_30 <- mapping[info_participant$biseleven_30]
# on enregistre 
write.csv(info_participant, "~/Desktop/tasks/pygame-python/Montpellier/experience-based/info_participant.csv", row.names = FALSE)

# planification (1,7,8,10,12,13,14,15,18,27,29) # mean score general population 23.6 +/- 4.5 for M and F Barratt 2009
planification <- info_participant$biseleven_1 + info_participant$biseleven_7 + info_participant$biseleven_8 + info_participant$biseleven_10 + info_participant$biseleven_12 + info_participant$biseleven_13 + info_participant$biseleven_15 + info_participant$biseleven_20 + info_participant$biseleven_29 
mean(planification) # 20.43
sd (planification) # 5.37

# impulsivité motrice (2,3,4,16,17,19,21,22,23,25,30) # mean score general population 22 +/- 4
motor <- info_participant$biseleven_2 + info_participant$biseleven_3 + info_participant$biseleven_4 + info_participant$biseleven_16 + info_participant$biseleven_17 + info_participant$biseleven_19 + info_participant$biseleven_21 + info_participant$biseleven_22 + info_participant$biseleven_23 + info_participant$biseleven_25 + info_participant$biseleven_30
mean(motor) # 20.96
sd(motor) # 3.21

# attention / impulsivité cognitive (5,6,9,11,20,24,26,28) # mean score general population 16.8 +/- 3.9
attention <- info_participant$biseleven_5 + info_participant$biseleven_6 + info_participant$biseleven_11 + info_participant$biseleven_20 + info_participant$biseleven_24 + info_participant$biseleven_26 + info_participant$biseleven_28
mean(attention) # 15.56
sd(attention) # 2.61


### BIS-BAS
# 1 tout à fait vrai/d'accord --> 4 pas du tout vrai/d'accord
# inverser le score de tous les items sauf le 2 et 22
mapping <- c(4,3,2,1)
info_participant$bisbas_1 <- mapping[info_participant$bisbas_1]
info_participant$bisbas_3 <- mapping[info_participant$bisbas_3]
info_participant$bisbas_4 <- mapping[info_participant$bisbas_4]
info_participant$bisbas_5 <- mapping[info_participant$bisbas_5]
info_participant$bisbas_6 <- mapping[info_participant$bisbas_6]
info_participant$bisbas_7 <- mapping[info_participant$bisbas_7]
info_participant$bisbas_8 <- mapping[info_participant$bisbas_8]
info_participant$bisbas_9 <- mapping[info_participant$bisbas_9]
info_participant$bisbas_10 <- mapping[info_participant$bisbas_10]
info_participant$bisbas_11 <- mapping[info_participant$bisbas_11]
info_participant$bisbas_12 <- mapping[info_participant$bisbas_12]
info_participant$bisbas_13 <- mapping[info_participant$bisbas_13]
info_participant$bisbas_14 <- mapping[info_participant$bisbas_14]
info_participant$bisbas_15 <- mapping[info_participant$bisbas_15]
info_participant$bisbas_16 <- mapping[info_participant$bisbas_16]
info_participant$bisbas_17 <- mapping[info_participant$bisbas_17]
info_participant$bisbas_18 <- mapping[info_participant$bisbas_18]
info_participant$bisbas_19 <- mapping[info_participant$bisbas_19]
info_participant$bisbas_20 <- mapping[info_participant$bisbas_20]
info_participant$bisbas_21 <- mapping[info_participant$bisbas_21]
info_participant$bisbas_23 <- mapping[info_participant$bisbas_23]
info_participant$bisbas_24 <- mapping[info_participant$bisbas_24]

# on enregistre 
write.csv(info_participant, "~/Desktop/tasks/pygame-python/Montpellier/description-based/info_participant.csv", row.names = FALSE)

# les items 1, 6, 11 et 17 sont des items de remplissage et ne sont pas inclus dans le calcul des scores
# BIS (2,8,13,16,19,22,24) # mean healthy population 19.9+/-3.8
bis <- info_participant$bisbas_2 + info_participant$bisbas_8 + info_participant$bisbas_13 + info_participant$bisbas_16 + info_participant$bisbas_19 + info_participant$bisbas_22 + info_participant$bisbas_24
mean(bis) #19.39
sd(bis) #3.8

# reward responsivness (4,7,14,18,23) # mean healthy population 17.59+/-2.14
reward_response <- info_participant$bisbas_4 + info_participant$bisbas_7 + info_participant$bisbas_14 + info_participant$bisbas_18 + info_participant$bisbas_23
mean(reward_response) # 17.65
sd(reward_response) # 1.9

# drive (3,9,12,21) # mean healthy population 12.05+/-2.36 
drive <- info_participant$bisbas_3 + info_participant$bisbas_9 + info_participant$bisbas_12 + info_participant$bisbas_21
mean(drive) # 10.39
sd(drive) #2.39

# fun seeking (5,10,15,20) # mean healthy population 12.43+/-2.36
fun_seek <- info_participant$bisbas_5 + info_participant$bisbas_10 + info_participant$bisbas_15 + info_participant$bisbas_20
mean(fun_seek) # 12.0
sd(fun_seek) # 2.3

############################# analyse des stratégies déclarées ############################
install.packages(c("tm", "wordcloud", "tidyverse", "textdata", "tidytext", "topicmodels", "RColorBrewer"))


# Chargement des bibliothèques
library(dplyr)
library(tm)
library(wordcloud)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(RColorBrewer)

# Liste personnalisée de stopwords étendus
stopwords_custom <- c("ma", "mon", "mes", "ton", "tes", "ta", "sa", "ses", "notre", "nos", "votre", "vos",
                      "je", "tu", "il", "elle", "nous", "vous", "ils", "elles", 
                      "me", "te",
                      "et", "ou", "mais", "donc", "or", "ni", "car", "de", "la", "le", "un", "une", 
                      "des", "en", "à", "au", "du", "sur", "où", "avec", "ce", "cet", "cette", 
                      "que", "qui", "quoi", "dont", "là", "les", "au", "aux", "dans", "y", "à", "par", "puis", "ensuite", "fois",
                      "pour", "sans", "sous", "plus", "moins", "tous", "toutes", "tout", "comme", "si", "se", "on", "ai", "a", "est", "j", "d", "n", "pas","l", 
                      "séquence","début", "moyens", "moyennes", "bouton", "été", "fait", "100", "afin", "soit", "selon", "clic", "durant")

# Nettoyage des stratégies
strategies_clean <- info_participant %>%
  select(task_3_quest_strategie) %>%                       # Sélection de la colonne des stratégies
  mutate(text = tolower(task_3_quest_strategie)) %>%       # Mise en minuscules
  mutate(text = gsub("[[:punct:]]", " ", text)) %>%        # Suppression de la ponctuation
  unnest_tokens(word, text) %>%                           # Tokenisation : un mot par ligne
  filter(!word %in% stopwords_custom) %>%                 # Suppression des mots non essentiels
  count(word, sort = TRUE)                                # Comptage des mots

# Affichage des 10 mots les plus fréquents
print(head(strategies_clean, 20))

# Visualisation des mots fréquents
ggplot(strategies_clean[1:15,], aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Mots les plus fréquents dans les stratégies", x = "Mots", y = "Fréquence")

##Création des nuages de mots par catégorie
# Fonction pour générer un nuage de mots pour une catégorie
# et filter les mots qui nous intéressent pas 

# Liste des mots à exclure (articles, déterminants, adverbes, etc.)
stopwords_custom <- c("ma", "mon", "mes", "ton", "tes", "ta", "sa", "ses", "notre", "nos", "votre", "vos", "moi", "toi", "ne", "quelque", "quelques", 
                      "je", "tu", "il", "elle", "nous", "vous", "ils", "elles", "tout", "tous", "toute", "toutes", "points",
                      "me", "te", "et", "ou", "mais", "donc", "or", "ni", "car", "de", "la", "le", "un", "une", 
                      "des", "en", "à", "au", "du", "sur", "où", "avec", "ce", "cet", "cette", 
                      "que", "qui", "quoi", "dont", "là", "les", "au", "aux", "dans", "y", "à", "par", "puis", 
                      "ensuite", "fois", "pour", "sans", "sous", "plus", "moins", "tous", "toutes", "tout", "comme", 
                      "si", "se", "on", "ai", "a", "est", "j", "d", "n", "m", "pas", "l", "séquence", "début", "moyens", 
                      "moyen", "moyenne","entre", "autre", "peu", "ayant", "que", "qu", "cela", "ce", "cet", "ces", "cette", "c", "eu",
                      "moyennes", "bouton", "été", "fait", "afin", "soit", "selon", "clic", "durant", "faire", "aurait", "aurais", "fin", "deux", 
                      "petit", "petits", "gros", "grosse", "grosses","même", "suis", "ainsi", "aussi", "temps", "cas", "bien",
                      "100", "333", "5575", "10", "50", "4", "3000", "2600", "ecu" 
                    )

# Nettoyage et regroupement thématique des stratégies
info_participant_clean <- info_participant %>%
  mutate(task_3_quest_strategie = tolower(task_3_quest_strategie)) %>%
  mutate(task_3_quest_strategie = gsub("[[:punct:]]", " ", task_3_quest_strategie)) %>% # Enlever la ponctuation
  mutate(task_3_quest_strategie = gsub("\\s+", " ", task_3_quest_strategie)) %>% # Réduire les espaces
  mutate(task_3_quest_strategie = gsub("black swan|blackspown", "éviter black swan", task_3_quest_strategie)) %>%
  mutate(task_3_quest_strategie = gsub("jackpot|jack pot", "chercher jackpot", task_3_quest_strategie)) %>%
  mutate(task_3_quest_strategie = gsub("violet", "aller violet", task_3_quest_strategie)) %>%
  mutate(task_3_quest_strategie = gsub("orange", "aller orange", task_3_quest_strategie)) %>%
  mutate(task_3_quest_strategie = gsub("bleu", "éviter bleu", task_3_quest_strategie)) %>%
  mutate(task_3_quest_strategie = gsub("gains moyens|moyens gains", "gains moyens", task_3_quest_strategie)) %>%
  mutate(task_3_quest_strategie = gsub("petits gains|faibles gains", "gains faibles", task_3_quest_strategie)) %>%
  mutate(task_3_quest_strategie = gsub("pertes moyennes|moyennes pertes", "pertes moyennes", task_3_quest_strategie)) %>%
  mutate(task_3_quest_strategie = gsub("tester|explorer|essayer de comprendre", "tester options", task_3_quest_strategie)) %>%
  mutate(task_3_quest_strategie = gsub("limiter pertes|réduire pertes|minimiser pertes", "minimiser pertes", task_3_quest_strategie))

# Attribution des catégories enrichies
info_participant_clean <- info_participant_clean %>%
  mutate(theme = case_when(
    grepl("éviter black swan", task_3_quest_strategie) ~ "Éviter le Black Swan",
    grepl("chercher jackpot", task_3_quest_strategie) ~ "Chercher le Jackpot",
    grepl("gains moyens", task_3_quest_strategie) ~ "Privilégier les gains moyens",
    grepl("minimiser pertes", task_3_quest_strategie) ~ "Minimiser les pertes",
    grepl("tester options", task_3_quest_strategie) ~ "Explorer ou tester les options",
    grepl("aller violet", task_3_quest_strategie) ~ "Aller sur le Violet",
    grepl("aller orange", task_3_quest_strategie) ~ "Aller sur l'Orange",
    TRUE ~ "Autres stratégies"
  ))

# Comptage des stratégies complètes (n-grams)
# avec une vérification des données
# Fonction pour générer un nuage de mots avec des expressions entières (n-grams)
generate_wordcloud_with_n_grams <- function(data, theme_label) {
  
  # Sélectionner les stratégies de la catégorie spécifique
  strategies <- data %>%
    filter(theme == theme_label) %>%
    unnest_tokens(ngram, task_3_quest_strategie, token = "ngrams", n = 2) %>%  # Utiliser des bigrams (paires de mots)
    filter(!ngram %in% stopwords_custom)  # Filtrer les mots stop
  
  # Vérification du nombre de n-grams générés
  print(paste("Nombre de n-grams pour", theme_label, ":", nrow(strategies)))
  
  # Comptage des n-grams
  strategies_count <- strategies %>%
    count(ngram, sort = TRUE) %>%
    filter(n > 1)  # Ne garder que les n-grams qui apparaissent plus d'une fois
  
  # Vérification du comptage
  print(paste("Nombre de n-grams après comptage pour", theme_label, ":", nrow(strategies_count)))
  
  # Vérifier si la catégorie contient des données
  if (nrow(strategies_count) > 0) {
    # Nuage de mots pour les n-grams
    wordcloud(words = strategies_count$ngram, 
              freq = strategies_count$n, 
              min.freq = 1, 
              max.words = 100, 
              colors = brewer.pal(8, "Dark2"), 
              scale = c(3, 0.5))
  } else {
    # Afficher un message si aucune donnée n'est disponible
    print(paste("Aucune donnée disponible pour la catégorie:", theme_label))
  }
}

# Exemple de nuage de mots pour une catégorie
generate_wordcloud_with_strategies(info_participant_clean, "Éviter le Black Swan")
generate_wordcloud_with_strategies(info_participant_clean, "Chercher le Jackpot")
generate_wordcloud_with_strategies(info_participant_clean, "Privilégier les gains moyens")
generate_wordcloud_with_strategies(info_participant_clean, "Minimiser les pertes")
generate_wordcloud_with_strategies(info_participant_clean, "Explorer ou tester les options")

# avec des expressions en entier 
# Liste des expressions à conserver en entier
expressions <- c("éviter le black swan", "chercher le jackpot", "privilégier les gains moyens", 
                 "minimiser les pertes", "explorer ou tester les options", "éviter le bleu", "aller sur le violet")

# Fonction pour regrouper les expressions spécifiques
regroup_expressions <- function(text, expressions) {
  for (expression in expressions) {
    # Remplacer chaque expression par un mot-clé unique
    text <- gsub(paste0("\\b", expression, "\\b"), gsub(" ", "_", expression), text, ignore.case = TRUE)
  }
  return(text)
}

# Fonction pour générer un nuage de mots avec des expressions entières
generate_wordcloud_with_expressions <- function(data, theme_label) {
  
  # Sélectionner les stratégies de la catégorie spécifique
  strategies <- data %>%
    filter(theme == theme_label) %>%
    mutate(task_3_quest_strategie = sapply(task_3_quest_strategie, regroup_expressions, expressions = expressions)) %>%  # Regrouper les expressions
    unnest_tokens(word, task_3_quest_strategie, token = "words") %>%  # Tokenisation
    filter(!word %in% stopwords_custom)  # Filtrer les mots stop
  
  # Comptage des mots
  strategies_count <- strategies %>%
    count(word, sort = TRUE) %>%
    filter(n > 1)  # Ne garder que les mots qui apparaissent plus d'une fois
  
  # Vérification du comptage
  print(paste("Nombre de mots après comptage pour", theme_label, ":", nrow(strategies_count)))
  
  # Vérifier si la catégorie contient des données
  if (nrow(strategies_count) > 0) {
    # Nuage de mots pour les expressions
    wordcloud(words = strategies_count$word, 
              freq = strategies_count$n, 
              min.freq = 1, 
              max.words = 100, 
              colors = brewer.pal(8, "Dark2"), 
              scale = c(3, 0.5))
  } else {
    # Afficher un message si aucune donnée n'est disponible
    print(paste("Aucune donnée disponible pour la catégorie:", theme_label))
  }
}

# Exemple de nuage de mots pour une catégorie
generate_wordcloud_with_strategies(info_participant_clean, "Éviter le Black Swan")
generate_wordcloud_with_strategies(info_participant_clean, "Chercher le Jackpot")
generate_wordcloud_with_strategies(info_participant_clean, "Privilégier les gains moyens")
generate_wordcloud_with_strategies(info_participant_clean, "Minimiser les pertes")
generate_wordcloud_with_strategies(info_participant_clean, "Explorer ou tester les options")

############################### fonctions ###########################################

### EXtraction autour du REE
extract_REE <- function(id, dataframe, ree_value, n_clics, limit){
  apparition_ree = which(dataframe[dataframe$id ==id,]$button_value == ree_value) # donne les lignes d'apparition du REE
  if(length(apparition_ree) == 0) {
    return(limit) # limit vaut 1 si pas BS et 100 si pas JP
  }
  apparition_ree <- unlist(lapply(apparition_ree, function(rang_ree){ # filter pour ne pas regarder les REE qui seraient apparus trop tot ou trop tard (pour l'instant)
    if(rang_ree >= n_clics + 1 & rang_ree <= 400 - n_clics) {
      rang_ree # liste des rang qui sont convenables avec retirer les rangs qui ne conviennent pas
    }
  }))
  return(apparition_ree)
}

# avoir les choix avant et les choix après chaque REE 
get_temp_series_around_ree <- function(apparition_ree, id, dataframe, n_clics){
  lapply(apparition_ree, function(rang_ree){
    autour_ree <- dataframe[dataframe$id == id,]$button_name[(rang_ree - n_clics):(rang_ree + n_clics)]
  })
}

# avoir les 10 choix avant et les 10 choix après chaque REE avec leur position
autour_ree_stats <- function(rang_ree, id, dataframe, n_clics){
  autour_ree <- dataframe[dataframe$id == id,]
  autour_ree$prop_temp <- cumsum(autour_ree$button_name == "ree")/seq(1,50)
  autour_ree$log_prop <- log(autour_ree$prop_temp/(1-autour_ree$prop_temp))
  autour_ree <- autour_ree[(rang_ree - n_clics):(rang_ree + n_clics),]
  autour_ree$is_ree <- c(rep(0, n_clics), 1, rep(0, n_clics))
  autour_ree$position = as.factor(c(rep("avant", n_clics+1), rep("après", n_clics)))
  autour_ree$trials = 1:(2 * n_clics+1)
  return(autour_ree)
}

###################### proportion globale  ######################################
# bloc 1 (sequence =0) ; bloc 2 (sequence=1)
data_fourchoices_1 <- data_fourchoices[data_fourchoices$sequence == 0,]
data_fourchoices_2 <- data_fourchoices[data_fourchoices$sequence == 1,]

data = data_fourchoices_exp_c
TSREE <- rep(0,length(unique(data$participant)) ) # axe des y
OSSREE<- rep(0,length(unique(data$participant)) ) # axe des x
# changer data et n
n = 400
for (i in unique(data$id)){
  print(i)
  print(table(data[data$id == i , ]$button_name))
  print(table(data[data$id == i , ]$button_name)/n)
  prop_choice = table(data[data$id == i , ]$button_name)/n
  OSSREE[i] = prop_choice[4]-prop_choice[3] # f vulnérable - f robuste
  TSREE[i] = 1 + prop_choice[1]-prop_choice[2] # 1 + f antifragile - f fragile
}

plot(OSSREE,TSREE,col="black",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)

llabels=seq(1,length(OSSREE))
for (i in 1:length(OSSREE)){text(OSSREE[i],TSREE[i]-.05,llabels[i])}

########################### DÉBUT VS FIN 10 premiers choix vs 10 dernier choix ##############################

# pour chaque blocs 0 = premier bloc ; 1 = 2 ème bloc 
data = data_fourchoices_2
n = 200
n_clic = 10

labbels = list()
OSSREE_premiers_choix = list()
OSSREE_derniers_choix = list()
TSREE_premiers_choix = list()
TSREE_derniers_choix = list()
for (i in unique(data$id)){
  print(i)
  premiers_choix = data$button_name[data$id == i][1:n_clic]
  prop_premiers_choix = table(premiers_choix)/length(premiers_choix)
  print(prop_premiers_choix)
  derniers_choix = data$button_name[data$id == i][n-n_clic:n]
  prop_derniers_choix = table(derniers_choix)/length(derniers_choix)
  print(prop_derniers_choix)
  OSSREE_premiers_choix = append(OSSREE_premiers_choix, prop_premiers_choix[4]-prop_premiers_choix[3])
  OSSREE_derniers_choix = append(OSSREE_derniers_choix, prop_derniers_choix[4]-prop_derniers_choix[3])
  TSREE_premiers_choix = append(TSREE_premiers_choix, 1 + prop_premiers_choix[1]-prop_premiers_choix[2])
  TSREE_derniers_choix = append(TSREE_derniers_choix, 1+ prop_derniers_choix[1]-prop_derniers_choix[2])
  labbels = append(labbels, i)
}
labbels = unlist(labbels)
OSSREE_premiers_choix = unlist(OSSREE_premiers_choix)
OSSREE_derniers_choix = unlist(OSSREE_derniers_choix)
TSREE_premiers_choix = unlist(TSREE_premiers_choix)
TSREE_derniers_choix = unlist(TSREE_derniers_choix)

plot(OSSREE_premiers_choix, TSREE_premiers_choix, col = "red", pch="+", cex=2,xlim=c(-1,1),ylim=c(0,2), xlab = "OSSREE", ylab = "TSREE")
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)
title(main = "first vs last choices")
points(OSSREE_derniers_choix,TSREE_derniers_choix, col = "black", pch="+", cex=2)

for (i in 1:length(OSSREE_premiers_choix)){text(OSSREE_premiers_choix[i],TSREE_premiers_choix[i]-.05,labbels[i])}
for (i in 1:length(OSSREE_derniers_choix)){text(OSSREE_derniers_choix[i],TSREE_derniers_choix[i]-.05,labbels[i])}

######################### moyenne mobile et dynamique des participants ########################

# pour chaque blocs 0 = premier bloc ; 1 = 2 ème bloc 
data = data_fourchoices_descr

# moyenne mobile 
moy.mob.10 <- function(p){
  mm <- rep(0,(length(p)-10))
  for (i in 10:length(p)){mm[i] <- sum(p[(i-9):i]) / 10}
  return(mm)}

# pour le OSREE et le TSREE : 
# OSREE p = cumsum(data_1$button_name == "vulnerable")/seq(1,200) - cumsum(data_1$button_name == "robuste")/seq(1,200)
# TSREE p = 1 + cumsum(data_1$button_name == "antifragile")/seq(1,200) - cumsum(data_1$button_name == "fragile")/seq(1,200)
# enlever les valeurs 0 avec data[-seq(1,9)]
# globale
moy.mobile.sensitivity <- function(data, id, n){
  OSSREE = moy.mob.10(cumsum(data[data$id == id,]$button_name == "vulnerable")/seq(1,n) - cumsum(data[data$id == id,]$button_name == "robuste")/seq(1,n))
  TSREE = moy.mob.10(1 + cumsum(data[data$id == id,]$button_name == "antifragile")/seq(1,n) - cumsum(data[data$id == id,]$button_name == "fragile")/seq(1,n))
  data_sensitivity = data.frame(OSSREE = OSSREE[-seq(1,9)], TSREE = TSREE[-seq(1,9)])
  return(data_sensitivity)
}

#plot
for (i in unique(data$id)){
  data_frame = moy.mobile.sensitivity(data, i, 400)
  plot(data_frame[,1], data_frame[,2], type = 'l', lwd = 3, col = "hotpink3",xlim=c(-1,1),ylim=c(0,2), xlab = "OSSREE", ylab = "TSREE")
  lines(c(0,1,0,-1,0),c(0,1,2,1,0))
  lines(c(0,0),c(0,2),lty=2)
  lines(c(-1,1),c(1,1),lty=2)
  title(main = paste("Mean choice of participant ", i, " through trials"))
}

# plot avec des flèches et des points rouge pour BS et vert pour JP 
# Ajouter des points rouges, verts et des flèches sur le graphe
for (i in unique(data$id)) {
  # Calculer les sensibilités
  data_frame = moy.mobile.sensitivity(data, i, 400)
  
  # Tracer le graphique de base
  plot(data_frame[,1], data_frame[,2], 
       type = 'l', 
       lwd = 3, 
       col = "hotpink3",
       xlim = c(-1, 1), 
       ylim = c(0, 2), 
       xlab = "OSSREE", 
       ylab = "TSREE")
  
  # Ajouter des lignes de référence
  lines(c(0, 1, 0, -1, 0), c(0, 1, 2, 1, 0))      # Contour
  lines(c(0, 0), c(0, 2), lty = 2)                # Ligne verticale
  lines(c(-1, 1), c(1, 1), lty = 2)               # Ligne horizontale
  
  # Ajouter un titre
  title(main = paste("Mean choice of participant ", i, " through trials"))
  
  # Extraire les positions des 3000 et -3000
  positions_3000 <- extract_REE(i, data, 3000, 10, 100)      # Positions pour les 3000
  positions_neg3000 <- extract_REE(i, data, -3000, 10, 1)    # Positions pour les -3000
  
  # Ajouter les points verts (pour 3000) si positions valides
  if (!(length(positions_3000) == 1 && positions_3000 == 100)) {
    for (pos in positions_3000) {
      points(data_frame$OSSREE[pos - 9], data_frame$TSREE[pos - 9], col = "green", pch = 19, cex = 1.5)
    }
  }
  
  # Ajouter les points rouges (pour -3000) si positions valides
  if (!(length(positions_neg3000) == 1 && positions_neg3000 == 1)) {
    for (pos in positions_neg3000) {
      points(data_frame$OSSREE[pos - 9], data_frame$TSREE[pos - 9], col = "red", pch = 19, cex = 1.5)
    }
  }
}


# local avec de l'oublie
moy.mob.10.choix.OSREE <- function(data, id){
  mm <- rep(0,190)
  for (i in 10:190){mm[i] <- (sum(data[data$id == id,]$button_name[(i-9):i] == "vulnerable") / 10) - (sum(data[data$id == id,]$button_name[(i-9):i] == "robuste") / 10)}
  return(mm[-seq(1,9)])}

moy.mob.10.choix.TSREE<- function(data, id){
  mm <- rep(0,190)
  for (i in 10:190){mm[i] <- 1 + (sum(data[data$id == id,]$button_name[(i-9):i] == "antifragile") / 10) - (sum(data[data$id == id,]$button_name[(i-9):i] == "fragile") / 10)}
  return(mm[-seq(1,9)])}

moy.mobile.sensitivity.local <- function(data, id){
  OSSREE = moy.mob.10.choix.OSREE(data, id)
  TSREE = moy.mob.10.choix.TSREE(data,id)
  data_sensitivity = data.frame(OSSREE = OSSREE, TSREE = TSREE)
  return(data_sensitivity)
}

#plot LOCAL
data = data_fourchoices[data_fourchoices$sequence == 0,]
for (i in unique(data$id)){
  data_frame = moy.mobile.sensitivity.local(data, i)
  plot(data_frame[,1], data_frame[,2], type = 'l', col = "hotpink4",xlim=c(-1,1),ylim=c(0,2), xlab = "OSSREE", ylab = "TSREE")
  lines(c(0,1,0,-1,0),c(0,1,2,1,0))
  lines(c(0,0),c(0,2),lty=2)
  lines(c(-1,1),c(1,1),lty=2)
  title(main = paste("Moyenne mobile des choix du participant ", i, " en fonction des essais - preference - V4"))
}

### pour chacune des options 
# moyenne mobile 
moy.mob.10 <- function(p){
  mm <- rep(0,(length(p)-10))
  for (i in 10:length(p)){mm[i] <- sum(p[(i-9):i]) / 10}
  return(mm)}

# Ajouter des points rouges, verts et des flèches sur le graphe
for (i in unique(data$id)) {
  # Calculer les sensibilités
  prop_antifragile <- moy.mob.10(cumsum(data[data$id == i,]$button_name == "antifragile")/seq(1,200) )
  prop_fragile <- moy.mob.10(cumsum(data[data$id == i,]$button_name == "fragile")/seq(1,200) )
  prop_robust <- moy.mob.10(cumsum(data[data$id == i,]$button_name == "robuste")/seq(1,200) )
  prop_vulnerable <- moy.mob.10(cumsum(data[data$id == i,]$button_name == "vulnerable")/seq(1,200) )

  # Tracer le graphique de base
  plot(prop_antifragile, type = 'l', lwd = 3, col = "chartreuse4",  xlim=c(0,200), ylim = c(0, 1), xlab = "trials", 
       ylab = "proportion")
  lines(prop_fragile, col = "orange",lwd = 3, xlim=c(0,200))      
  lines(prop_robust, col = "darkcyan",lwd = 3, xlim=c(0,200))        
  lines(prop_vulnerable, col = "darkmagenta",lwd = 3, xlim=c(0,200))              
  # Ajouter un titre
  title(main = paste("Proportion of choice of participant ", i, " through trials"))
  
  # Extrait les positions où JP a été vu
  positions_JP <- extract_REE(i, data, 3000, 10, 100)
  # Extrait les positions où BS a été vu
  positions_BS <- extract_REE(i, data, -3000, 10, 1)
  # Ajouter des barres verticales vertes si JP
  if (!(length(positions_JP) == 1 && positions_JP == 100)) {
    abline(v = positions_JP, col = "green", lty = 2, lwd = 2)}
  # Ajouter des barres verticales rouges si BS
  if (!(length(positions_BS) == 1 && positions_BS == 1)) {
    abline(v = positions_BS, col = "red", lty = 2, lwd = 2)}
}


### Diversification
diversification.i <- function(p){
  s1 <- 0
  s2 <- 0
  V4 <- 0
  s4 <- 0
  if (p[1]>0){s1 <- p[1]*log(p[1])}
  if (p[2]>0){s2 <- p[2]*log(p[2])}
  if (p[3]>0){V4 <- p[3]*log(p[3])}
  if (p[4]>0){s4 <- p[4]*log(p[4])}
  return(exp(-s1-s2-V4-s4))}

# p = vecteur de choix data[data$id == number,]$option
# diversification cumulée => match les fréquences 
diversification.cum <- function(p){
  diversification.cum = rep(0,200)
  for (i in 1:200){
    pp = c(cumsum(p == "antifragile")[i]/i,cumsum(p == "fragile")[i]/i, cumsum(p == "robuste")[i]/i, cumsum(p == "vulnerable")[i]/i)
    print(pp)
    diversification.cum[i] = diversification.i(pp)
  }
  return(diversification.cum)
}

#plot pour tous les participants 
data = data_fourchoices_1
for (i in unique(data$id)){
  plot(diversification.cum(data[data$id == i,]$button_name), 
       xlim = c(10, 200), ylim = c(1,4), 
       type = 'l', xlab = "trials", ylab = "diversification score")
  title(main = paste("Diversification score of participant", i, " through trials"))
  
  # Extrait les positions où JP a été vu
  positions_JP <- extract_REE(i, data, 3000, 10, 100)
  # Extrait les positions où BS a été vu
  positions_BS <- extract_REE(i, data, -3000, 10, 1)
  # Ajouter des barres verticales vertes si JP
  if (!(length(positions_JP) == 1 && positions_JP == 100)) {
    abline(v = positions_JP, col = "green", lty = 2, lwd = 2)}
  # Ajouter des barres verticales rouges si BS
  if (!(length(positions_BS) == 1 && positions_BS == 1)) {
    abline(v = positions_BS, col = "red", lty = 2, lwd = 2)}
}

# moyenne mobile / diversification locale
diversification.w <- function(p){
  diversification.w  <-  rep(0,190)
  for (i in 10:200){
    pp  <-  c(sum(p[(i-9):i]=="antifragile"),
              sum(p[(i-9):i]=="robuste"),
              sum(p[(i-9):i]=="vulnerable"),
              sum(p[(i-9):i]=="fragile"))/10
    print(pp)
    diversification.w[i-9]  <-  diversification.i(pp)}
  return(round(diversification.w,1))}


#plot pour tous les participants 

for (i in unique(data$id)){
  plot(diversification.w(data[data$id == i,]$button_name), 
       xlim = c(10, 200), ylim = c(1,4), 
       type = 'l', xlab = "n°trials", ylab = "diversification score")
  title(main = paste("Diversification score of participant ", i, " through trials"))
  
  # Extrait les positions où JP a été vu
  positions_JP <- extract_REE(i, data, 3000, 10, 100)
  # Extrait les positions où BS a été vu
  positions_BS <- extract_REE(i, data, -3000, 10, 1)
  # Ajouter des barres verticales vertes si JP
  if (!(length(positions_JP) == 1 && positions_JP == 100)) {
    abline(v = positions_JP, col = "green", lty = 2, lwd = 2)}
  # Ajouter des barres verticales rouges si BS
  if (!(length(positions_BS) == 1 && positions_BS == 1)) {
    abline(v = positions_BS, col = "red", lty = 2, lwd = 2)}
}

###### pour les choix mais au temps t :
for (i in unique(data$id)) {
  # Sélectionner les données pour le participant
  participant_data <- data[data$id == i, ]
  
  # Créer un graphique avec type = 'l' et une séquence explicite pour l'axe des x
  plot(x = 1:nrow(participant_data),  y = participant_data$button_name, type = "l", xlab = "n°trials", ylab = "choice", 
    main = paste("Choices of participant ", i, " through trials")
  )
  # Extrait les positions où JP a été vu
  positions_JP <- extract_REE(i, data, 3000, 10, 100)
  # Extrait les positions où BS a été vu
  positions_BS <- extract_REE(i, data, -3000, 10, 1)
  # Ajouter des barres verticales vertes si JP
  if (!(length(positions_JP) == 1 && positions_JP == 100)) {
    abline(v = positions_JP, col = "green", lty = 2, lwd = 2)}
  # Ajouter des barres verticales rouges si BS
  if (!(length(positions_BS) == 1 && positions_BS == 1)) {
    abline(v = positions_BS, col = "red", lty = 2, lwd = 2)}
}


#################################### calcul des élasticités BSA JPS NOF ##########################

# Black Swan Avoidance BSA A + R 
# JackPot Seeking JPS A + V 
# NOt Fragile NOF T - F = A + V + R 

# moyenne [t-delta, t] moyenne mobile locale avec oublie selon delta et le button (à mettre en " ")
# p = data$button_name pour un participant
moy.mob.local.delta.button <- function(p, delta, button){
  mm <- rep(0,(length(p)-delta))
  for (i in delta:length(p)){mm[i-delta+1] <- sum(p[(i-delta+1):i]== button) / delta}
  return(mm)}

# moyenne T moyenne mobile cumulée 
# p = data$button_name pour un participant ; button est le nom du bouton en " "
moy.mob.cumul.button <- function(p, button){
  mm <- rep(0,length(p))
  for (i in 1:length(p)){mm[i] <- cumsum(p == button)[i] / i}
  return(mm)
}

# n [t-delta, t] moyenne mobile locale avec oublie selon delta et le button (à mettre en " ")
# p = data$button_name pour un participant
n.mob.local.delta.button <- function(p, delta, button){
  mm <- rep(0,(length(p)-delta))
  for (i in delta:length(p)){mm[i-delta+1] <- sum(p[(i-delta+1):i]== button)}
  return(mm)}

# Nt moyenne mobile cumulée 
# p = data$button_name pour un participant ; button est le nom du bouton en " "
N.mob.cumul.button <- function(p, button){
  mm <- rep(0,length(p))
  for (i in 1:length(p)){mm[i] <- cumsum(p == button)[i]}
  return(mm)
}

#####  CLUSTERING  #####
BSA_sup_JPS <- c(1,6,8,10,12,13,16,17,20,23,24,25,27,28,29,30,31,32,33,36,37,38,41,46,47,49,51,52,56,58,60,63,64,65,66,67,68,69,70,79,80)
BSA_inf_JPS <- c(9,11,26,34,39,40,42,43,61,62,71,72,74,75,76,77)
BSA_equals_JPS <- c(2,3,4,5,7,14,15,18,19,21,22,35,44,45,48,50,53,54,55,57,59,73,78)


R_A <- c(1,8,12,16,21,23,24,25,27,30,32,36,38,41,44,45,46,49,56,58,60,64,65,66,67,68,69,78,80)
A_V <- c(2,9,11,13,15,17,18,26,39,42,43,52,53,54,57,71,73,77,79)
V_F <- c(3,6,14,19,22,34,37,47,61,62,70,72,75,74,76)
R_F <- c(4,10,20,28,29,31,35,63)
middle_mixing <- c(5,7,33,40,48,50,51,55,59)

# --- paramètres ---
id_list <- middle_mixing
delta <- 10

# --- Nom du fichier PDF ---
pdf_file <- "plots_BSA_JPS_NOF_middle_mixing.pdf"
# --- Ouvre le PDF pour enregistrer les graphiques ---
pdf(pdf_file, width = 10, height = 8)
# --- 4 graphiques par page ---
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
count <- 0

for (i in id_list) {
  p <- data_fourchoices_descr[data_fourchoices_descr$id == i,]$button_name
  # --- Calculs des trois ratios ---
  BSA <- ( moy.mob.local.delta.button(p, delta, "antifragile") + moy.mob.local.delta.button(p, delta, "robuste") ) / ( moy.mob.cumul.button(p, "antifragile")[-seq(1,(delta-1))] + moy.mob.cumul.button(p, "robuste")[-seq(1,(delta-1))] )
  JPS <- ( moy.mob.local.delta.button(p, delta, "antifragile") + moy.mob.local.delta.button(p, delta, "vulnerable") ) / ( moy.mob.cumul.button(p, "antifragile")[-seq(1,(delta-1))] + moy.mob.cumul.button(p, "vulnerable")[-seq(1,(delta-1))] )
  NOF <- ( moy.mob.local.delta.button(p, delta, "antifragile") + moy.mob.local.delta.button(p, delta, "robuste") + moy.mob.local.delta.button(p, delta, "vulnerable") ) / ( moy.mob.cumul.button(p, "antifragile")[-seq(1,(delta-1))] + moy.mob.cumul.button(p, "robuste")[-seq(1,9)] + moy.mob.cumul.button(p, "vulnerable")[-seq(1,9)] )
  # --- Création du plot ---
  matplot(1:length(BSA), cbind(BSA, JPS, NOF), type = "l", lty = 1, lwd = 2,
          col = c("black", "darkcyan", "darkorange"),
          xlab = "trials", ylab = "BSA (black), JPS (blue), NOF (orange)", main = paste("Cluster mixing - Participant", i))
  #legend("topright", legend = c("BSA", "JPS", "NOF"),
         #col = c("black", "darkcyan", "darkorange", lwd = 2))
  
  positions_JP <- extract_REE(i, data, 3000, 10, 100)
  positions_BS <- extract_REE(i, data, -3000, 10, 1)
  # Barres verticales
  if (!(length(positions_JP) == 1 && positions_JP == 100)) {
    abline(v = positions_JP, col = "green", lty = 2, lwd = 0.5)}
  if (!(length(positions_BS) == 1 && positions_BS == 1)) {
    abline(v = positions_BS, col = "red", lty = 2, lwd = 0.5)}

  # --- Gestion des pages : 4 plots par page ---
  count <- count + 1
  if (count %% 4 == 0) { par(mfrow = c(2, 2)) } }

# --- Ferme le PDF ---
dev.off()
# --- Ouvre automatiquement le PDF (macOS) ---
system(paste("open", pdf_file))


################################### majorité de choix A / F / R / V ? ###########################

# donne le nom de l'option max directement en chr donc on met en factor
# as.factor(names(which.max(table(data_fourchoices$button_name))))

avant_BS <- list()
apres_BS <- list()
avant_JP <- list()
apres_JP <- list()

apparition_BS <- rep(0,3)
apparition_JP <- rep(0,3)
data = data_fourchoices_exp_2
n = 200
for (i in unique(data$id)) {
  apparition_BS = extract_REE(i, data, -3000, 10 , 1)
  apparition_JP = extract_REE(i, data, 3000, 10, 100)
  print(i)
  print(apparition_BS)
  print(apparition_JP)
  four_choice_i = data[data$id == i , ]$button_name
  
  if (apparition_BS[1] != 1){
    for (k in 1:length(apparition_BS)) {
      before = four_choice_i[as.integer(apparition_BS[k]-20):apparition_BS[k]]
      after = four_choice_i[apparition_BS[k]:as.integer(apparition_BS[k]+19)]
      option_max_before_BS = as.factor(names(which.max(table(before))))
      #print(option_max_before_BS)
      option_max_after_BS = as.factor(names(which.max(table(after))))
      #print(option_max_after_BS)
      avant_BS <- append(avant_BS, option_max_before_BS)
      apres_BS <- append(apres_BS, option_max_after_BS)
    }}
  
  if (apparition_JP[1] != 100){
    for (l in 1:length(apparition_JP)) {
      before = four_choice_i[as.integer(apparition_JP[l]-20):apparition_JP[l]]
      after = four_choice_i[apparition_JP[l]:as.integer(apparition_JP[l]+19)]
      option_max_before_JP = as.factor(names(which.max(table(before))))
      #print(option_max_before_JP)
      option_max_after_JP = as.factor(names(which.max(table(after))))
      #print(option_max_after_JP)
      avant_JP <- append(avant_JP, option_max_before_JP)
      apres_JP <- append(apres_JP, option_max_after_JP)
    }}
}

avant_BS <- unlist(avant_BS)
apres_BS <- unlist(apres_BS)
avant_JP <- unlist(avant_JP)
apres_JP <- unlist(apres_JP)

# max avant vs après BS et JP 
prop.table(table(avant_BS))
prop.table(table(apres_BS))
prop.table(table(avant_JP))
prop.table(table(apres_JP))

# tests 
mcnemar.test(table(avant_BS, apres_BS))
nominalSymmetryTest(table(avant_BS, apres_BS)) # celui là est apparié
mcnemar.test(table(avant_JP, apres_JP))
nominalSymmetryTest(table(avant_JP, apres_JP))

# Fonction pour faire l'analyse McNemar binaire pour une variable avant après pour chacune des options
analyser_mcnemar <- function(avant, apres, nom) {
  cat("\n=====================", nom, "=====================\n")
  categories <- levels(avant)
  
  for (cat in categories) {
    avant_prop <- mean(avant == cat)
    apres_prop <- mean(apres == cat)
    
    # Créer variables binaires pour le test McNemar
    avant_bin <- ifelse(avant == cat, 1, 0)
    apres_bin <- ifelse(apres == cat, 1, 0)
    test <- mcnemar.test(table(avant_bin, apres_bin))
    
    cat("\nCatégorie :", cat,
        "\nProportion avant :", round(avant_prop, 3),
        "\nProportion après :", round(apres_prop, 3),
        "\nP-value (McNemar) :", round(test$p.value, 4), "\n")
  }
}
# BS
analyser_mcnemar(avant_BS, apres_BS, "BS")
# JP
analyser_mcnemar(avant_JP, apres_JP, "JP")

# plot globaux avant / après par options
# Forcer l'ordre des niveaux
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# --- Fonction pour préparer les données pour le plot ---
prepare_plot_data <- function(avant, apres, nom) {
  n <- length(avant)
  data_long <- data.frame(
    participant = 1:n,
    avant_col = avant,
    apres_col = apres
  ) %>%
    pivot_longer(
      cols = c("avant_col", "apres_col"),
      names_to = "moment",
      values_to = "categorie"
    ) %>%
    mutate(
      # Recode pour avoir juste "avant" et "apres"
      moment = recode(moment, "avant_col" = "avant", "apres_col" = "apres"),
      moment = factor(moment, levels = c("avant", "apres"))
    )
  
  # Calcul proportions et erreur standard
  plot_data <- data_long %>%
    group_by(moment, categorie) %>%
    summarise(
      n_cat = n(),
      prop = n_cat / n,
      se = sqrt(prop * (1 - prop) / n),
      .groups = "drop"
    ) %>%
    mutate(variable = nom)
  
  return(plot_data)
}

# --- Préparer les données pour BS et JP ---
plot_BS <- prepare_plot_data(avant_BS, apres_BS, "BS")
plot_JP <- prepare_plot_data(avant_JP, apres_JP, "JP")

# --- Combiner les deux ---
plot_data <- bind_rows(plot_BS, plot_JP)

# --- Plot ggplot ---
ggplot(plot_data, aes(x = categorie, y = prop, fill = moment)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = prop - se, ymax = prop + se),
                position = position_dodge(width = 0.7), width = 0.2) +
  facet_wrap(~variable) +
  labs(
    y = "Proportion ± SE",
    x = "Option",
    title = "Evolution of proportions of options before vs after a REE",
    fill = "Moment"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = percent_format(accuracy = 1))


# PLot REE par REE avant après l'évolution ça passse de quoi à quoi 
# et en quelles proportions
library(ggalluvial)
library(ggplot2)
library(dplyr)

# Palette demandée (hex pour couleurs vives et stables)
my_colors <- c(
  "antifragile" = "#006400",  # darkgreen
  "robuste"     = "#008B8B",  # darkcyan
  "vulnerable"  = "#8B008B",  # darkmagenta
  "fragile"     = "#FF8C00"   # darkorange
)

# Fonction utilitaire pour tracer un alluvial propre
plot_alluvial <- function(avant, apres, title_text) {
  df <- data.frame(avant = avant, apres = apres) %>%
    mutate(
      avant = factor(avant, levels = c("antifragile", "robuste", "vulnerable", "fragile")),
      apres = factor(apres,  levels = c("antifragile", "robuste", "vulnerable", "fragile"))
    )
  
  ggplot(df, aes(axis1 = avant, axis2 = apres)) +
    # Alluvium : fill par "avant" (couleurs demandées), alpha élevé pour vibrance, pas de contours
    geom_alluvium(aes(fill = avant), width = 0.25, alpha = 0.95, colour = NA) +
    # Strates : remplissage clair + bord sombre fin pour lisibilité
    geom_stratum(width = 0.25, fill = "white", color = "grey40", size = 0.3) +
    # Labels des strates
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3.5) +
    # Forcer l'ordre des positions x (gauche = avant, droite = apres)
    scale_x_discrete(labels = c("Before", "After"), expand = c(.15, .05)) +
    # Palette manuelle (hex)
    scale_fill_manual(values = my_colors) +
    labs(title = title_text, x = NULL, y = NULL, fill = "Category") +
    # Fond blanc et suppression grilles pour un rendu net comme ton code initial
    theme_minimal(base_size = 12) +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(face = "bold"),
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 14)
    )
}

# Tracer BS
p_bs <- plot_alluvial(avant_BS, apres_BS, "Changes of options after a BS")
print(p_bs)

# Tracer JP
p_jp <- plot_alluvial(avant_JP, apres_JP, "Changes of options after a JP")
print(p_jp)


######################################## temps de réaction / de choix ##############################

data = data_fourchoices[data_fourchoices$sequence==1,]
all_interclics <- c()
for (i in unique(data$id)){
  interclics <- diff(data[data$id==i,]$time_since_loaded)-1 #présentation de l'outcome dure 1s
  # Ajout des interclics du participant au vecteur global
  all_interclics <- c(all_interclics, interclics)
  cat("i:", i, 
      "Mean:", mean(interclics), 
      "SD:", sd(interclics), 
      "Max:", max(interclics), 
      "Min:", min(interclics), "\n")
  plot(interclics, xlab= "essais", ylab="temps du choix (s)", pch="+")
  title(main = paste(" Temps de choix du participant ", i, " en fonction des essais"))
}

cat("global:", mean(all_interclics), "global sd", sd(all_interclics), "min", min(all_interclics), "max", max(all_interclics), "\n")

######################## decision selon la séquence de REE vécu #################################

# bloc 1
data_fourchoices_1 <- data_fourchoices[data_fourchoices$sequence == 0,]
# bloc 2
data_fourchoices_2 <- data_fourchoices[data_fourchoices$sequence == 1,]

# the ones that only experienced only one JP
OSSREE_before_JP = list()
TSREE_before_JP = list()
OSSREE_after_JP = list()
TSREE_after_JP = list()
llabels_JP = list()
# the ones that only experienced 2 JPs
OSSREE_before_JP1 = list()
TSREE_before_JP1 = list()
OSSREE_between_JPs = list()
TSREE_between_JPs = list()
OSSREE_after_JP2 = list()
TSREE_after_JP2 = list()
llabels_2JP = list()
# the ones that only experienced 3 JPs
OSSREE_before_JP1s = list()
TSREE_before_JP1s = list()
OSSREE_between_JP1_JP2 = list()
TSREE_between_JP1_JP2 = list()
OSSREE_between_JP2_JP3 = list()
TSREE_between_JP2_JP3 = list()
OSSREE_after_JP3 = list()
TSREE_after_JP3 = list()
llabels_3JP = list()
#the ones that only experienced only one BS
OSSREE_before_BS = list()
TSREE_before_BS = list()
OSSREE_after_BS = list()
TSREE_after_BS = list()
llabels_BS = list()
# the ones that only experienced 2 BSs
OSSREE_before_BS1 = list()
TSREE_before_BS1 = list()
OSSREE_between_BSs = list()
TSREE_between_BSs = list()
OSSREE_after_BS2 = list()
TSREE_after_BS2 = list()
llabels_2BS = list()
# the ones that only experienced 3 BSs
OSSREE_before_BS1s = list()
TSREE_before_BS1s = list()
OSSREE_between_BS1_BS2 = list()
TSREE_between_BS1_BS2 = list()
OSSREE_between_BS2_BS3 = list()
TSREE_between_BS2_BS3 = list()
OSSREE_after_BS3 = list()
TSREE_after_BS3 = list()
llabels_3BS = list()
## the ones that experienced exactly 1 JP and 1 BS
# BS then JP
OSSREE_before_BS_BSJP = list()
TSREE_before_BS_BSJP = list()
OSSREE_between_BSJP = list()
TSREE_between_BSJP = list()
OSSREE_after_JP_BSJP = list()
TSREE_after_JP_BSJP = list()
llabels_BSJP = list()
# JP then BS 
OSSREE_before_JP_JPBS = list()
TSREE_before_JP_JPBS = list()
OSSREE_between_JPBS = list()
TSREE_between_JPBS = list()
OSSREE_after_BS_JPBS = list()
TSREE_after_BS_JPBS = list()
llabels_JPBS = list()
## the ones that experienced 2 JP and 1 BS
# BS1 JP1 JP2
OSSREE_before_BS1_Ree = list()
TSREE_before_BS1_Ree = list()
OSSREE_between_BS1JP1_Ree = list()
TSREE_between_BS1JP1_Ree = list()
OSSREE_between_JP1JP2_Ree = list()
TSREE_between_JP1JP2_Ree = list()
OSSREE_after_JP2_Ree = list()
TSREE_after_JP2_Ree = list()
llabels_BS1JP1JP2 = list()
# JP1 BS2 JP2
OSSREE_before_JP1_reE = list()
TSREE_before_JP1_reE = list()
OSSREE_between_JP1BS2_reE = list()
TSREE_between_JP1BS2_reE = list()
OSSREE_between_BS2JP2_reE = list()
TSREE_between_BS2JP2_reE = list()
OSSREE_after_JP2_reE = list()
TSREE_after_JP2_reE = list()
llabels_JP1BS2JP2 = list()
# JP1 JP2 BS
OSSREE_before_JP1_reEE = list()
TSREE_before_JP1_reEE = list()
OSSREE_between_JP1JP2_reEE = list()
TSREE_between_JP1JP2_reEE = list()
OSSREE_between_JP2BS_reEE = list()
TSREE_between_JP2BS_reEE = list()
OSSREE_after_BS_reEE = list()
TSREE_after_BS_reEE = list()
llabels_JP1JP2BS = list()

## the ones that experienced 2 BS and 1 JP 
# BS1 JP1 BS2
OSSREE_before_BS1_REe = list()
TSREE_before_BS1_REe = list()
OSSREE_between_BS1JP1_REe = list()
TSREE_between_BS1JP1_REe = list()
OSSREE_between_JP1BS2_REe = list()
TSREE_between_JP1BS2_REe = list()
OSSREE_after_BS2_REe = list()
TSREE_after_BS2_REe = list()
llabels_BS1JP1BS2 = list()
# BS1 BS2 JP2
OSSREE_before_BS1_ree = list()
TSREE_before_BS1_ree = list()
OSSREE_between_BS1BS2_ree = list()
TSREE_between_BS1BS2_ree = list()
OSSREE_between_BS2JP2_ree = list()
TSREE_between_BS2JP2_ree = list()
OSSREE_after_JP2_ree = list()
TSREE_after_JP2_ree = list()
llabels_BS1BS2JP2 = list()
# JP BS1 BS2
OSSREE_before_JP_reee = list()
TSREE_before_JP_reee = list()
OSSREE_between_JPBS1_reee = list()
TSREE_between_JPBS1_reee = list()
OSSREE_between_BS1BS2_reee = list()
TSREE_between_BS1BS2_reee = list()
OSSREE_after_BS2_reee = list()
TSREE_after_BS2_reee = list()
llabels_JPBS1BS2 = list()

## the ones experienced 2 JP and 2 BS
# BS1 JP1 BS2 JP2
OSSREE_before_BS1_rEE = list()
TSREE_before_BS1_rEE = list()
OSSREE_between_BS1JP1_rEE = list()
TSREE_between_BS1JP1_rEE = list()
OSSREE_between_JP1BS2_rEE = list()
TSREE_between_JP1BS2_rEE = list()
OSSREE_between_BS2JP2_rEE = list()
TSREE_between_BS2JP2_rEE = list()
OSSREE_after_JP2_rEE = list()
TSREE_after_JP2_rEE = list()
llabels_BS1JP1BS2JP2 = list()
# JP1 BS1 JP2 BS2 
OSSREE_before_JP1_rEE = list()
TSREE_before_JP1_rEE = list()
OSSREE_between_JP1BS1_rEE = list()
TSREE_between_JP1BS1_rEE = list()
OSSREE_between_BS1JP2_rEE = list()
TSREE_between_BS1JP2_rEE = list()
OSSREE_between_JP2BS2_rEE = list()
TSREE_between_JP2BS2_rEE = list()
OSSREE_after_BS2_rEE = list()
TSREE_after_BS2_rEE = list()
llabels_JP1BS2JP2BS2 = list()

### the ones experienced 3 JP and 2 BS
OSSREE_before_JP1_REE = list()
TSREE_before_JP1_REE = list()
OSSREE_between_JP1BS1_REE = list()
TSREE_between_JP1BS1_REE = list()
OSSREE_between_BS1JP2_REE = list()
TSREE_between_BS1JP2_REE = list()
OSSREE_between_JP2BS2_REE = list()
TSREE_between_JP2BS2_REE = list()
OSSREE_between_BS2JP3_REE = list()
TSREE_between_BS2JP3_REE = list()
OSSREE_after_JP3_REE = list()
TSREE_after_JP3_REE = list()
llabels_JP1BS1JP2BS2JP3 = list()
## the ones experienced 3 BS and 2 JP
OSSREE_before_BS1_REE = list()
TSREE_before_BS1_REE = list()
OSSREE_between_BS1JP1_REE = list()
TSREE_between_BS1JP1_REE = list()
OSSREE_between_JP1BS2_REE = list()
TSREE_between_JP1BS2_REE = list()
OSSREE_between_BS2JP2_REE = list()
TSREE_between_BS2JP2_REE = list()
OSSREE_between_JP2BS3_REE = list()
TSREE_between_JP2BS3_REE = list()
OSSREE_after_BS3_REE = list()
TSREE_after_BS3_REE = list()
llabels_BS1JP1BS2JP3BS3 = list()
## the ones experienced 3JP 1BS 
# JP1 BS JP2 JP3 
OSSREE_before_JP1_REEe = list()
TSREE_before_JP1_REEe = list()
OSSREE_between_JP1BS_REEe = list()
TSREE_between_JP1BS_REEe = list()
OSSREE_between_BSJP2_REEe = list()
TSREE_between_BSJP2_REEe = list()
OSSREE_between_JP2JP3_REEe = list()
TSREE_between_JP2JP3_REEe = list()
OSSREE_after_JP3_REEe = list()
TSREE_after_JP3_REEe = list()
llabels_JP1BSJP2JP3 = list()
# JP1 JP2 BS JP3
OSSREE_before_JP1_REEE = list()
TSREE_before_JP1_REEE = list()
OSSREE_between_JP1JP2_REEE = list()
TSREE_between_JP1JP2_REEE = list()
OSSREE_between_JP2BS_REEE = list()
TSREE_between_JP2BS_REEE = list()
OSSREE_between_BSJP3_REEE = list()
TSREE_between_BSJP3_REEE = list()
OSSREE_after_JP3_REEE = list()
TSREE_after_JP3_REEE = list()
llabels_JP1JP2BSJP3 = list()
### the ones experienced 3BS 1JP
# BS1 JP BS2 BS3
OSSREE_before_BS1_REEe = list()
TSREE_before_BS1_REEe = list()
OSSREE_between_BS1JP_REEe = list()
TSREE_between_BS1JP_REEe = list()
OSSREE_between_JPBS2_REEe = list()
TSREE_between_JPBS2_REEe = list()
OSSREE_between_BS2BS3_REEe = list()
TSREE_between_BS2BS3_REEe = list()
OSSREE_after_BS3_REEe = list()
TSREE_after_BS3_REEe = list()
llabels_BS1JPBS2BS3 = list()
# BS1 BS2 JP BS3
OSSREE_before_BS1_REEE = list()
TSREE_before_BS1_REEE = list()
OSSREE_between_BS1BS2_REEE = list()
TSREE_between_BS1BS2_REEE = list()
OSSREE_between_BS2JP_REEE = list()
TSREE_between_BS2JP_REEE = list()
OSSREE_between_JPBS3_REEE = list()
TSREE_between_JPBS3_REEE = list()
OSSREE_after_BS3_REEE = list()
TSREE_after_BS3_REEE = list()
llabels_BS1BS2JPBS3 = list()
## the ones that experienced none
OSSREE_noneREE = list()
TSREE_noneREE = list()
llabels_none = list()

## évolution des choix en fonction des REE vécus
apparition_BS <- rep(0,3)
apparition_JP <- rep(0,3)

# modifier nombre de participants ; data set ; max 100 ou 200
data = data_fourchoices_2
n = 200
for (i in unique(data$id)){
  apparition_BS = extract_REE(i, data, -3000, 10 , 1)
  apparition_JP = extract_REE(i, data, 3000, 10, 100)
  print(i)
  print(apparition_BS)
  print(apparition_JP)
  print(length(apparition_BS))
  print(length(apparition_JP))
  four_choice_i = data[data$id == i , ]$button_name
  
  # au moins 1 JP mais aucun BS
  if (apparition_BS[1] == 1 && apparition_JP[1]!= 100){ 
    # un seul JP
    if(length(apparition_JP)==1){ # un seul JP
      llabels_JP <- append(llabels_JP, i)
      before_JP = four_choice_i[1:apparition_JP[1]]
      after_JP = four_choice_i[apparition_JP[1]:n]
      print(table(before_JP)/length(before_JP))
      prop_choice_before_JP = table(before_JP)/length(before_JP)
      print(table(after_JP)/length(after_JP))
      prop_choice_after_JP = table(after_JP)/length(after_JP)
      OSSREE_before_JP <- append(OSSREE_before_JP, prop_choice_before_JP[4]-prop_choice_before_JP[3]) # f vulnérable - f robuste
      TSREE_before_JP <- append(TSREE_before_JP, 1 + prop_choice_before_JP[1]-prop_choice_before_JP[2]) # 1 + f antifragile - f fragile)
      OSSREE_after_JP <- append(OSSREE_after_JP, prop_choice_after_JP[4]-prop_choice_after_JP[3]) # f vulnérable - f robuste
      TSREE_after_JP <- append(TSREE_after_JP, 1 + prop_choice_after_JP[1]-prop_choice_after_JP[2]) # 1 + f antifragile - f fragile)
      
    }
    # 2 JP
    if(length(apparition_JP)==2){ # 2 JP 
      llabels_2JP <- append(llabels_2JP, i)
      before_JP1 = four_choice_i[1:apparition_JP[1]]
      between_JPs = four_choice_i[apparition_JP[1]:apparition_JP[2]]
      after_JP2 = four_choice_i[apparition_JP[2]:n]
      print(table(before_JP1)/length(before_JP1))
      prop_choice_before_JP1 = table(before_JP1)/length(before_JP1)
      print(table(between_JPs)/length(between_JPs))
      prop_choice_between_JPs = table(between_JPs)/length(between_JPs)
      print(table(after_JP2)/length(after_JP2))
      prop_choice_after_JP2 = table(after_JP2)/length(after_JP2)
      OSSREE_before_JP1 <- append(OSSREE_before_JP1, prop_choice_before_JP1[4]-prop_choice_before_JP1[3]) # f vulnérable - f robuste
      TSREE_before_JP1 <- append(TSREE_before_JP1, 1 + prop_choice_before_JP1[1]-prop_choice_before_JP1[2]) # 1 + f antifragile - f fragile)
      OSSREE_between_JPs <- append(OSSREE_between_JPs, prop_choice_between_JPs[4]-prop_choice_between_JPs[3])# f vulnérable - f robuste
      TSREE_between_JPs <- append(TSREE_between_JPs, 1 + prop_choice_between_JPs[1]-prop_choice_between_JPs[2]) # 1 + f antifragile - f fragile)
      OSSREE_after_JP2 <- append(OSSREE_after_JP2, prop_choice_after_JP2[4]-prop_choice_after_JP2[3]) # f vulnérable - f robuste
      TSREE_after_JP2 <- append(TSREE_after_JP2, 1 + prop_choice_after_JP2[1]-prop_choice_after_JP2[2]) # 1 + f antifragile - f fragile)
    }
    # 3 JP
    if (length(apparition_JP)==3){
      llabels_3JP <- append(llabels_3JP, i)
      before_JP1s = four_choice_i[1:apparition_JP[1]]
      between_JP1JP2 = four_choice_i[apparition_JP[1]:apparition_JP[2]]
      between_JP2JP3 = four_choice_i[apparition_JP[2]:apparition_JP[3]]
      after_JP3 = four_choice_i[apparition_JP[3]:n]
      print(table(before_JP1s)/length(before_JP1s))
      prop_choice_before_JP1 = table(before_JP1s)/length(before_JP1s)
      print(table(between_JP1JP2)/length(between_JP1JP2))
      prop_choice_between_JP1JP2 = table(between_JP1JP2)/length(between_JP1JP2)
      print(table(between_JP2JP3)/length(between_JP2JP3))
      prop_choice_between_JP2JP3 = table(between_JP2JP3)/length(between_JP2JP3)
      print(table(after_JP3)/length(after_JP3))
      prop_choice_after_JP3 = table(after_JP3)/length(after_JP3)
      OSSREE_before_JP1s <- append(OSSREE_before_JP1s, prop_choice_before_JP1[4]-prop_choice_before_JP1[3]) # f vulnérable - f robuste
      TSREE_before_JP1s <- append(TSREE_before_JP1s, 1 + prop_choice_before_JP1[1]-prop_choice_before_JP1[2]) # 1 + f antifragile - f fragile)
      OSSREE_between_JP1_JP2 <- append(OSSREE_between_JP1_JP2, prop_choice_between_JP1JP2[4]-prop_choice_between_JP1JP2[3])# f vulnérable - f robuste
      TSREE_between_JP1_JP2 <- append(TSREE_between_JP1_JP2, 1 + prop_choice_between_JP1JP2[1]-prop_choice_between_JP1JP2[2]) # 1 + f antifragile - f fragile)
      OSSREE_between_JP2_JP3 <- append(OSSREE_between_JP2_JP3, prop_choice_between_JP2JP3[4]-prop_choice_between_JP2JP3[3])# f vulnérable - f robuste
      TSREE_between_JP2_JP3 <- append(TSREE_between_JP2_JP3, 1 + prop_choice_between_JP2JP3[1]-prop_choice_between_JP2JP3[2]) # 1 + f antifragile - f fragile)
      OSSREE_after_JP3 <- append(OSSREE_after_JP3, prop_choice_after_JP3[4]-prop_choice_after_JP3[3]) # f vulnérable - f robuste
      TSREE_after_JP3 <- append(TSREE_after_JP3, 1 + prop_choice_after_JP3[1]-prop_choice_after_JP3[2]) # 1 + f antifragile - f fragile)

    } 
      
  }
  
  # au moins un BS mais aucun JP 
  if (apparition_JP[1] == 100 && apparition_BS[1] != 1){ 
    # un seul BS
    if (length(apparition_BS) == 1){
      llabels_BS <- append(llabels_BS, i)
      before_BS = four_choice_i[1:apparition_BS[1]]
      after_BS = four_choice_i[apparition_BS[1]:n]
      print(table(before_BS)/length(before_BS))
      prop_choice_before_BS = table(before_BS)/length(before_BS)
      print(table(after_BS)/length(after_BS))
      prop_choice_after_BS = table(after_BS)/length(after_BS)
      OSSREE_before_BS <- append(OSSREE_before_BS, prop_choice_before_BS[4]-prop_choice_before_BS[3]) # f vulnérable - f robuste
      TSREE_before_BS <- append(TSREE_before_BS, 1 + prop_choice_before_BS[1]-prop_choice_before_BS[2]) # 1 + f antifragile - f fragile)
      OSSREE_after_BS <- append(OSSREE_after_BS, prop_choice_after_BS[4]-prop_choice_after_BS[3]) # f vulnérable - f robuste
      TSREE_after_BS <- append(TSREE_after_BS, 1 + prop_choice_after_BS[1]-prop_choice_after_BS[2]) # 1 + f antifragile - f fragile)
      
    }
    # 2 BS
    if (length(apparition_BS) == 2){
      llabels_2BS <- append(llabels_2BS, i)
      before_BS1 = four_choice_i[1:apparition_BS[1]]
      between_BSs = four_choice_i[apparition_BS[1]:apparition_BS[2]]
      after_BS2 = four_choice_i[apparition_BS[2]:n]
      print(table(before_BS1)/length(before_BS1))
      prop_choice_before_BS1 = table(before_BS1)/length(before_BS1)
      print(table(between_BSs)/length(between_BSs))
      prop_choice_between_BSs = table(between_BSs)/length(between_BSs)
      print(table(after_BS2)/length(after_BS2))
      prop_choice_after_BS2 = table(after_BS2)/length(after_BS2)
      OSSREE_before_BS1 <- append(OSSREE_before_BS1, prop_choice_before_BS1[4]-prop_choice_before_BS1[3]) # f vulnérable - f robuste
      TSREE_before_BS1 <- append(TSREE_before_BS1, 1 + prop_choice_before_BS1[1]-prop_choice_before_BS1[2]) # 1 + f antifragile - f fragile)
      OSSREE_between_BSs <- append(OSSREE_between_BSs, prop_choice_between_BSs[4]-prop_choice_between_BSs[3]) # f vulnérable - f robuste
      TSREE_between_BSs <- append(TSREE_between_BSs, 1 + prop_choice_between_BSs[1]-prop_choice_between_BSs[2]) # 1 + f antifragile - f fragile)
      OSSREE_after_BS2 <- append(OSSREE_after_BS2, prop_choice_after_BS2[4]-prop_choice_after_BS2[3]) # f vulnérable - f robuste
      TSREE_after_BS2 <- append(TSREE_after_BS2, 1 + prop_choice_after_BS2[1]-prop_choice_after_BS2[2]) # 1 + f antifragile - f fragile)
      
    }
    # 3 BS 
    if (length(apparition_BS) == 3){
      llabels_3BS <- append(llabels_3BS, i)
      before_BS1 = four_choice_i[1:apparition_BS[1]]
      between_BS1BS2 = four_choice_i[apparition_BS[1]:apparition_BS[2]]
      between_BS2BS3 = four_choice_i[apparition_BS[2]:apparition_BS[3]]
      after_BS3 = four_choice_i[apparition_BS[3]:n]
      print(table(before_BS1)/length(before_BS1))
      prop_choice_before_BS1 = table(before_BS1)/length(before_BS1)
      print(table(between_BS1BS2)/length(between_BS1BS2))
      prop_choice_between_BS1BS2 = table(between_BS1BS2)/length(between_BS1BS2)
      print(table(between_BS2BS3)/length(between_BS2BS3))
      prop_choice_between_BS2BS3 = table(between_BS2BS3)/length(between_BS2BS3)
      print(table(after_BS3)/length(after_BS3))
      prop_choice_after_BS3 = table(after_BS3)/length(after_BS3)
      OSSREE_before_BS1s <- append(OSSREE_before_BS1s, prop_choice_before_BS1[4]-prop_choice_before_BS1[3]) # f vulnérable - f robuste
      TSREE_before_BS1s <- append(TSREE_before_BS1s, 1 + prop_choice_before_BS1[1]-prop_choice_before_BS1[2]) # 1 + f antifragile - f fragile)
      OSSREE_between_BS1_BS2 <- append(OSSREE_between_BS1_BS2, prop_choice_between_BS1BS2[4]-prop_choice_between_BS1BS2[3]) # f vulnérable - f robuste
      TSREE_between_BS1_BS2 <- append(TSREE_between_BS1_BS2, 1 + prop_choice_between_BS1BS2[1]-prop_choice_between_BS1BS2[2]) # 1 + f antifragile - f fragile)
      OSSREE_between_BS2_BS3 <- append(OSSREE_between_BS2_BS3, prop_choice_between_BS2BS3[4]-prop_choice_between_BS2BS3[3]) # f vulnérable - f robuste
      TSREE_between_BS2_BS3 <- append(TSREE_between_BS2_BS3, 1 + prop_choice_between_BS2BS3[1]-prop_choice_between_BS2BS3[2]) # 1 + f antifragile - f fragile)
      OSSREE_after_BS3 <- append(OSSREE_after_BS3, prop_choice_after_BS3[4]-prop_choice_after_BS3[3]) # f vulnérable - f robuste
      TSREE_after_BS3 <- append(TSREE_after_BS3, 1 + prop_choice_after_BS3[1]-prop_choice_after_BS3[2]) # 1 + f antifragile - f fragile)

    }
  }
  
  # si exatement un BS et un JP existant
  if (length(apparition_BS)==1 && length(apparition_JP)==1 && apparition_JP[1]!= 100 && apparition_BS[1] != 1){
    # si BS avant JP
    if (apparition_BS[1] < apparition_JP[1]){
      llabels_BSJP <- append(llabels_BSJP, i)
      before_BS = four_choice_i[1:apparition_BS[1]]
      between_BSJP = four_choice_i[apparition_BS[1]:apparition_JP[1]]
      after_JP = four_choice_i[apparition_JP[1]:n]
      print(table(before_BS)/length(before_BS))
      prop_choice_before_BS = table(before_BS)/length(before_BS)
      print(table(between_BSJP)/length(between_BSJP))
      prop_choice_between_BSJP = table(between_BSJP)/length(between_BSJP)
      print(table(after_JP)/length(after_JP))
      prop_choice_after_JP = table(after_JP)/length(after_JP)
      OSSREE_before_BS_BSJP <- append(OSSREE_before_BS_BSJP, prop_choice_before_BS[4]-prop_choice_before_BS[3]) # f vulnérable - f robuste
      TSREE_before_BS_BSJP <- append(TSREE_before_BS_BSJP, 1 + prop_choice_before_BS[1]-prop_choice_before_BS[2]) # 1 + f antifragile - f fragile)
      OSSREE_between_BSJP <- append(OSSREE_between_BSJP, prop_choice_between_BSJP[4]-prop_choice_between_BSJP[3])# f vulnérable - f robuste
      TSREE_between_BSJP <- append(TSREE_between_BSJP, 1 + prop_choice_between_BSJP[1]-prop_choice_between_BSJP[2])# 1 + f antifragile - f fragile)
      OSSREE_after_JP_BSJP <- append(OSSREE_after_JP_BSJP, prop_choice_after_JP[4]-prop_choice_after_JP[3]) # f vulnérable - f robuste
      TSREE_after_JP_BSJP <- append(TSREE_after_JP_BSJP, 1 + prop_choice_after_JP[1]-prop_choice_after_JP[2]) # 1 + f antifragile - f fragile)
      
    }
    # si JP avant BS
    if (apparition_BS[1] > apparition_JP[1]){
      llabels_JPBS <- append(llabels_JPBS, i)
      before_JP = four_choice_i[1:apparition_JP[1]]
      between_JPBS = four_choice_i[apparition_JP[1]:apparition_BS[1]]
      after_BS = four_choice_i[apparition_BS[1]:n]
      print(table(before_JP)/length(before_JP))
      prop_choice_before_JP = table(before_JP)/length(before_JP)
      print(table(between_JPBS)/length(between_JPBS))
      prop_choice_between_JPBS = table(between_JPBS)/length(between_JPBS)
      print(table(after_BS)/length(after_BS))
      prop_choice_after_BS = table(after_BS)/length(after_BS)
      OSSREE_before_JP_JPBS <- append(OSSREE_before_JP_JPBS, prop_choice_before_JP[4]-prop_choice_before_JP[3]) # f vulnérable - f robuste
      TSREE_before_JP_JPBS <- append(TSREE_before_JP_JPBS, 1 + prop_choice_before_JP[1]-prop_choice_before_JP[2]) # 1 + f antifragile - f fragile)
      OSSREE_between_JPBS <- append(OSSREE_between_JPBS, prop_choice_between_JPBS[4]-prop_choice_between_JPBS[3])# f vulnérable - f robuste
      TSREE_between_JPBS <- append(TSREE_between_JPBS, 1 + prop_choice_between_JPBS[1]-prop_choice_between_JPBS[2])# 1 + f antifragile - f fragile)
      OSSREE_after_BS_JPBS <- append(OSSREE_after_BS_JPBS, prop_choice_after_BS[4]-prop_choice_after_BS[3]) # f vulnérable - f robuste
      TSREE_after_BS_JPBS <- append(TSREE_after_BS_JPBS, 1 + prop_choice_after_BS[1]-prop_choice_after_BS[2]) # 1 + f antifragile - f fragile)
      
    }
  }
  
  # si 2 JP et 1 BS existant
  if (length(apparition_JP)==2 && length(apparition_BS) == 1 && apparition_BS[1] != 1){
    # est-ce que c'est BS1 dans ce cas c'est BS1 JP1 JP2
    # ou bien BS2 et dans ce cas c'est JP1 BS2 JP2
    
    # BS1 JP1 JP2
    if (apparition_JP[1] > apparition_BS[1]){
      llabels_BS1JP1JP2 <- append(llabels_BS1JP1JP2, i)
      before_BS1 = four_choice_i[1:apparition_BS[1]]
      between_BS1JP1 = four_choice_i[apparition_BS[1]:apparition_JP[1]]
      between_JP1JP2 = four_choice_i[apparition_JP[1]:apparition_JP[2]]
      after_JP2 = four_choice_i[apparition_JP[2]:n]
      print(table(before_BS1)/length(before_BS1))
      prop_choice_before_BS1 = table(before_BS1)/length(before_BS1)
      print(table(between_BS1JP1)/length(between_BS1JP1))
      prop_choice_between_BS1JP1 = table(between_BS1JP1)/length(between_BS1JP1)
      print(table(between_JP1JP2)/length(between_JP1JP2))
      prop_choice_between_JP1JP2 = table(between_JP1JP2)/length(between_JP1JP2)
      print(table(after_JP2)/length(after_JP2))
      prop_choice_after_JP2 = table(after_JP2)/length(after_JP2)
      OSSREE_before_BS1_Ree <- append(OSSREE_before_BS1_Ree, prop_choice_before_BS1[4]-prop_choice_before_BS1[3]) # f vulnérable - f robuste
      TSREE_before_BS1_Ree <- append(TSREE_before_BS1_Ree, 1 + prop_choice_before_BS1[1]-prop_choice_before_BS1[2]) # 1 + f antifragile - f fragile)
      OSSREE_between_BS1JP1_Ree <- append(OSSREE_between_BS1JP1_Ree, prop_choice_between_BS1JP1[4]-prop_choice_between_BS1JP1[3])# f vulnérable - f robuste
      TSREE_between_BS1JP1_Ree <- append(TSREE_between_BS1JP1_Ree, 1 + prop_choice_between_BS1JP1[1]-prop_choice_between_BS1JP1[2])# 1 + f antifragile - f fragile)
      OSSREE_between_JP1JP2_Ree <- append(OSSREE_between_JP1JP2_Ree, prop_choice_between_JP1JP2[4]-prop_choice_between_JP1JP2[3])# f vulnérable - f robuste
      TSREE_between_JP1JP2_Ree <- append(TSREE_between_JP1JP2_Ree, 1 + prop_choice_between_JP1JP2[1]-prop_choice_between_JP1JP2[2])# 1 + f antifragile - f fragile)
      OSSREE_after_JP2_Ree <- append(OSSREE_after_JP2_Ree, prop_choice_after_JP2[4]-prop_choice_after_JP2[3]) # f vulnérable - f robuste
      TSREE_after_JP2_Ree <- append(TSREE_after_JP2_Ree, 1 + prop_choice_after_JP2[1]-prop_choice_after_JP2[2]) # 1 + f antifragile - f fragile)
      
    }
    # JP1 BS2 JP2
    if(apparition_JP[1] < apparition_BS[1]){
      llabels_JP1BS2JP2 <- append(llabels_JP1BS2JP2, i)
      before_JP1 = four_choice_i[1:apparition_JP[1]]
      between_JP1BS2 = four_choice_i[apparition_JP[1]:apparition_BS[1]]
      between_BS2JP2 = four_choice_i[apparition_BS[1]:apparition_JP[2]]
      after_JP2 = four_choice_i[apparition_JP[2]:n]
      print(table(before_JP1)/length(before_JP1))
      prop_choice_before_JP1 = table(before_JP1)/length(before_JP1)
      print(table(between_JP1BS2)/length(between_JP1BS2))
      prop_choice_between_JP1BS2 = table(between_JP1BS2)/length(between_JP1BS2)
      print(table(between_BS2JP2)/length(between_BS2JP2))
      prop_choice_between_BS2JP2 = table(between_BS2JP2)/length(between_BS2JP2)
      print(table(after_JP2)/length(after_JP2))
      prop_choice_after_JP2 = table(after_JP2)/length(after_JP2)
      OSSREE_before_JP1_reE <- append(OSSREE_before_JP1_reE, prop_choice_before_JP1[4]-prop_choice_before_JP1[3]) # f vulnérable - f robuste
      TSREE_before_JP1_reE <- append(TSREE_before_JP1_reE, 1 + prop_choice_before_JP1[1]-prop_choice_before_JP1[2]) # 1 + f antifragile - f fragile)
      OSSREE_between_JP1BS2_reE <- append(OSSREE_between_JP1BS2_reE, prop_choice_between_JP1BS2[4]-prop_choice_between_JP1BS2[3])# f vulnérable - f robuste
      TSREE_between_JP1BS2_reE <- append(TSREE_between_JP1BS2_reE, 1 + prop_choice_between_JP1BS2[1]-prop_choice_between_JP1BS2[2])# 1 + f antifragile - f fragile)
      OSSREE_between_BS2JP2_reE <- append(OSSREE_between_BS2JP2_reE, prop_choice_between_BS2JP2[4]-prop_choice_between_BS2JP2[3])# f vulnérable - f robuste
      TSREE_between_BS2JP2_reE <- append(TSREE_between_BS2JP2_reE, 1 + prop_choice_between_BS2JP2[1]-prop_choice_between_BS2JP2[2])# 1 + f antifragile - f fragile)
      OSSREE_after_JP2_reE <- append(OSSREE_after_JP2_reE, prop_choice_after_JP2[4]-prop_choice_after_JP2[3]) # f vulnérable - f robuste
      TSREE_after_JP2_reE <- append(TSREE_after_JP2_reE, 1 + prop_choice_after_JP2[1]-prop_choice_after_JP2[2]) # 1 + f antifragile - f fragile)
      
    }
    # JP1 JP2 BS
    if(apparition_JP[2] < apparition_BS[1]){
      llabels_JP1JP2BS <- append(llabels_JP1JP2BS, i)
      before_JP1 = four_choice_i[1:apparition_JP[1]]
      between_JP1JP2 = four_choice_i[apparition_JP[1]:apparition_JP[2]]
      between_JP2BS = four_choice_i[apparition_JP[2]:apparition_BS[1]]
      after_BS = four_choice_i[apparition_BS[1]:n]
      print(table(before_JP1)/length(before_JP1))
      prop_choice_before_JP1 = table(before_JP1)/length(before_JP1)
      print(table(between_JP1JP2)/length(between_JP1JP2))
      prop_choice_between_JP1JP2 = table(between_JP1JP2)/length(between_JP1JP2)
      print(table(between_JP2BS)/length(between_JP2BS))
      prop_choice_between_JP2BS = table(between_JP2BS)/length(between_JP2BS)
      print(table(after_BS)/length(after_BS))
      prop_choice_after_BS = table(after_BS)/length(after_BS)
      OSSREE_before_JP1_reEE <- append(OSSREE_before_JP1_reEE, prop_choice_before_JP1[4]-prop_choice_before_JP1[3]) # f vulnérable - f robuste
      TSREE_before_JP1_reEE <- append(TSREE_before_JP1_reEE, 1 + prop_choice_before_JP1[1]-prop_choice_before_JP1[2]) # 1 + f antifragile - f fragile)
      OSSREE_between_JP1JP2_reEE <- append(OSSREE_between_JP1JP2_reEE, prop_choice_between_JP1JP2[4]-prop_choice_between_JP1JP2[3])# f vulnérable - f robuste
      TSREE_between_JP1JP2_reEE <- append(TSREE_between_JP1JP2_reEE, 1 + prop_choice_between_JP1JP2[1]-prop_choice_between_JP1JP2[2])# 1 + f antifragile - f fragile)
      OSSREE_between_JP2BS_reEE <- append(OSSREE_between_JP2BS_reEE, prop_choice_between_JP2BS[4]-prop_choice_between_JP2BS[3])# f vulnérable - f robuste
      TSREE_between_JP2BS_reEE <- append(TSREE_between_JP2BS_reEE, 1 + prop_choice_between_JP2BS[1]-prop_choice_between_JP2BS[2])# 1 + f antifragile - f fragile)
      OSSREE_after_BS_reEE <- append(OSSREE_after_BS_reEE, prop_choice_after_BS[4]-prop_choice_after_BS[3]) # f vulnérable - f robuste
      TSREE_after_BS_reEE <- append(TSREE_after_BS_reEE, 1 + prop_choice_after_BS[1]-prop_choice_after_BS[2]) # 1 + f antifragile - f fragile)
    }
    
  }
  # si 2 BS et 1 JP existant
  if (length(apparition_BS)==2 && length(apparition_JP) == 1 && apparition_JP[1]!= 100){
    # plusieurs possibilités dans ce cas là
    
    # BS1 JP1 BS2
    if (apparition_JP[1] < apparition_BS[2]){
      llabels_BS1JP1BS2 <- append(llabels_BS1JP1BS2, i)
      before_BS1 = four_choice_i[1:apparition_BS[1]]
      between_BS1JP1 = four_choice_i[apparition_BS[1]:apparition_JP[1]]
      between_JP1BS2 = four_choice_i[apparition_JP[1]:apparition_BS[2]]
      after_BS2 = four_choice_i[apparition_BS[2]:n]
      print(table(before_BS1)/length(before_BS1))
      prop_choice_before_BS1 = table(before_BS1)/length(before_BS1)
      print(table(between_BS1JP1)/length(between_BS1JP1))
      prop_choice_between_BS1JP1 = table(between_BS1JP1)/length(between_BS1JP1)
      print(table(between_JP1BS2)/length(between_JP1BS2))
      prop_choice_between_JP1BS2 = table(between_JP1BS2)/length(between_JP1BS2)
      print(table(after_BS2)/length(after_BS2))
      prop_choice_after_BS2 = table(after_BS2)/length(after_BS2)
      OSSREE_before_BS1_REe <- append(OSSREE_before_BS1_REe, prop_choice_before_BS1[4]-prop_choice_before_BS1[3]) # f vulnérable - f robuste
      TSREE_before_BS1_REe <- append(TSREE_before_BS1_REe, 1 + prop_choice_before_BS1[1]-prop_choice_before_BS1[2]) # 1 + f antifragile - f fragile)
      OSSREE_between_BS1JP1_REe <- append(OSSREE_between_BS1JP1_REe, prop_choice_between_BS1JP1[4]-prop_choice_between_BS1JP1[3])# f vulnérable - f robuste
      TSREE_between_BS1JP1_REe <- append(TSREE_between_BS1JP1_REe, 1 + prop_choice_between_BS1JP1[1]-prop_choice_between_BS1JP1[2])# 1 + f antifragile - f fragile)
      OSSREE_between_JP1BS2_REe <- append(OSSREE_between_JP1BS2_REe, prop_choice_between_JP1BS2[4]-prop_choice_between_JP1BS2[3])# f vulnérable - f robuste
      TSREE_between_JP1BS2_REe <- append(TSREE_between_JP1BS2_REe, 1 + prop_choice_between_JP1BS2[1]-prop_choice_between_JP1BS2[2])# 1 + f antifragile - f fragile)
      OSSREE_after_BS2_REe <- append(OSSREE_after_BS2_REe, prop_choice_after_BS2[4]-prop_choice_after_BS2[3]) # f vulnérable - f robuste
      TSREE_after_BS2_REe <- append(TSREE_after_BS2_REe, 1 + prop_choice_after_BS2[1]-prop_choice_after_BS2[2]) # 1 + f antifragile - f fragile)
      
    }
    # BS1 BS2 JP2
    if(apparition_JP[1] > apparition_BS[2]){
      llabels_BS1BS2JP2 <- append(llabels_BS1BS2JP2, i)
      before_BS1 = four_choice_i[1:apparition_BS[1]]
      between_BS1BS2 = four_choice_i[apparition_BS[1]:apparition_BS[2]]
      between_BS2JP2 = four_choice_i[apparition_BS[2]:apparition_JP[1]]
      after_JP2 = four_choice_i[apparition_JP[1]:n]
      print(table(before_BS1)/length(before_BS1))
      prop_choice_before_BS1 = table(before_BS1)/length(before_BS1)
      print(table(between_BS1BS2)/length(between_BS1BS2))
      prop_choice_between_BS1BS2 = table(between_BS1BS2)/length(between_BS1BS2)
      print(table(between_BS2JP2)/length(between_BS2JP2))
      prop_choice_between_BS2JP2 = table(between_BS2JP2)/length(between_BS2JP2)
      print(table(after_JP2)/length(after_JP2))
      prop_choice_after_JP2 = table(after_JP2)/length(after_JP2)
      OSSREE_before_BS1_ree <- append(OSSREE_before_BS1_ree, prop_choice_before_BS1[4]-prop_choice_before_BS1[3]) # f vulnérable - f robuste
      TSREE_before_BS1_ree <- append(TSREE_before_BS1_ree, 1 + prop_choice_before_BS1[1]-prop_choice_before_BS1[2]) # 1 + f antifragile - f fragile)
      OSSREE_between_BS1BS2_ree <- append(OSSREE_between_BS1BS2_ree, prop_choice_between_BS1BS2[4]-prop_choice_between_BS1BS2[3])# f vulnérable - f robuste
      TSREE_between_BS1BS2_ree <- append(TSREE_between_BS1BS2_ree, 1 + prop_choice_between_BS1BS2[1]-prop_choice_between_BS1BS2[2])# 1 + f antifragile - f fragile)
      OSSREE_between_BS2JP2_ree <- append(OSSREE_between_BS2JP2_ree, prop_choice_between_BS2JP2[4]-prop_choice_between_BS2JP2[3])# f vulnérable - f robuste
      TSREE_between_BS2JP2_ree <- append(TSREE_between_BS2JP2_ree, 1 + prop_choice_between_BS2JP2[1]-prop_choice_between_BS2JP2[2])# 1 + f antifragile - f fragile)
      OSSREE_after_JP2_ree <- append(OSSREE_after_JP2_ree, prop_choice_after_JP2[4]-prop_choice_after_JP2[3]) # f vulnérable - f robuste
      TSREE_after_JP2_ree <- append(TSREE_after_JP2_ree, 1 + prop_choice_after_JP2[1]-prop_choice_after_JP2[2]) # 1 + f antifragile - f fragile)
      
      
    }
    # JP BS1 BS2
    if (apparition_JP[1] < apparition_BS[1]){
      llabels_JPBS1BS2 <- append(llabels_JPBS1BS2, i)
      before_JP = four_choice_i[1:apparition_JP[1]]
      between_JPBS1 = four_choice_i[apparition_JP[1]:apparition_BS[1]]
      between_BS1BS2 = four_choice_i[apparition_BS[1]:apparition_BS[2]]
      after_BS2 = four_choice_i[apparition_BS[2]:n]
      print(table(before_JP)/length(before_JP))
      prop_choice_before_JP = table(before_JP)/length(before_JP)
      print(table(between_JPBS1)/length(between_JPBS1))
      prop_choice_between_JPBS1 = table(between_JPBS1)/length(between_JPBS1)
      print(table(between_BS1BS2)/length(between_BS1BS2))
      prop_choice_between_BS1BS2 = table(between_BS1BS2)/length(between_BS1BS2)
      print(table(after_BS2)/length(after_BS2))
      prop_choice_after_BS2 = table(after_BS2)/length(after_BS2)
      OSSREE_before_JP_reee <- append(OSSREE_before_JP_reee, prop_choice_before_JP[4]-prop_choice_before_JP[3]) # f vulnérable - f robuste
      TSREE_before_JP_reee <- append(TSREE_before_JP_reee, 1 + prop_choice_before_JP[1]-prop_choice_before_JP[2]) # 1 + f antifragile - f fragile)
      OSSREE_between_JPBS1_reee <- append(OSSREE_between_JPBS1_reee, prop_choice_between_JPBS1[4]-prop_choice_between_JPBS1[3])# f vulnérable - f robuste
      TSREE_between_JPBS1_reee <- append(TSREE_between_JPBS1_reee, 1 + prop_choice_between_JPBS1[1]-prop_choice_between_JPBS1[2])# 1 + f antifragile - f fragile)
      OSSREE_between_BS1BS2_reee <- append(OSSREE_between_BS1BS2_reee, prop_choice_between_BS1BS2[4]-prop_choice_between_BS1BS2[3])# f vulnérable - f robuste
      TSREE_between_BS1BS2_reee <- append(TSREE_between_BS1BS2_reee, 1 + prop_choice_between_BS1BS2[1]-prop_choice_between_BS1BS2[2])# 1 + f antifragile - f fragile)
      OSSREE_after_BS2_reee <- append(OSSREE_after_BS2_reee, prop_choice_after_BS2[4]-prop_choice_after_BS2[3]) # f vulnérable - f robuste
      TSREE_after_BS2_reee <- append(TSREE_after_BS2_reee, 1 + prop_choice_after_BS2[1]-prop_choice_after_BS2[2]) # 1 + f antifragile - f fragile)
    }
    
  }
  # si 2 BS et 2 JP 
  if (length(apparition_BS)==2 && length(apparition_JP) == 2){
    # BS1 JP1 BS2 JP2 ou bien 
    if(apparition_BS[1]<apparition_JP[1]){
      llabels_BS1JP1BS2JP2 <- append(llabels_BS1JP1BS2JP2, i)
      before_BS1 = four_choice_i[1:apparition_BS[1]]
      between_BS1JP1 = four_choice_i[apparition_BS[1]:apparition_JP[1]]
      between_JP1BS2 = four_choice_i[apparition_JP[1]:apparition_BS[2]]
      between_BS2JP2 = four_choice_i[apparition_BS[2]:apparition_JP[2]]
      after_JP2 = four_choice_i[apparition_JP[2]:n]
      print(table(before_BS1)/length(before_BS1))
      prop_choice_before_BS1 = table(before_BS1)/length(before_BS1)
      print(table(between_BS1JP1)/length(between_BS1JP1))
      prop_choice_between_BS1JP1 = table(between_BS1JP1)/length(between_BS1JP1)
      print(table(between_JP1BS2)/length(between_JP1BS2))
      prop_choice_between_JP1BS2 = table(between_JP1BS2)/length(between_JP1BS2)
      print(table(between_BS2JP2)/length(between_BS2JP2))
      prop_choice_between_BS2JP2 = table(between_BS2JP2)/length(between_BS2JP2)
      print(table(after_JP2)/length(after_JP2))
      prop_choice_after_JP2 = table(after_JP2)/length(after_JP2)
      OSSREE_before_BS1_rEE <- append(OSSREE_before_BS1_rEE, prop_choice_before_BS1[4]-prop_choice_before_BS1[3]) # f vulnérable - f robuste
      TSREE_before_BS1_rEE <- append(TSREE_before_BS1_rEE, 1 + prop_choice_before_BS1[1]-prop_choice_before_BS1[2]) # 1 + f antifragile - f fragile)
      OSSREE_between_BS1JP1_rEE <- append(OSSREE_between_BS1JP1_rEE, prop_choice_between_BS1JP1[4]-prop_choice_between_BS1JP1[3])# f vulnérable - f robuste
      TSREE_between_BS1JP1_rEE <- append(TSREE_between_BS1JP1_rEE, 1 + prop_choice_between_BS1JP1[1]-prop_choice_between_BS1JP1[2])# 1 + f antifragile - f fragile)
      OSSREE_between_JP1BS2_rEE <- append(OSSREE_between_JP1BS2_rEE, prop_choice_between_JP1BS2[4]-prop_choice_between_JP1BS2[3])# f vulnérable - f robuste
      TSREE_between_JP1BS2_rEE <- append(TSREE_between_JP1BS2_rEE, 1 + prop_choice_between_JP1BS2[1]-prop_choice_between_JP1BS2[2])# 1 + f antifragile - f fragile)
      OSSREE_between_BS2JP2_rEE <- append(OSSREE_between_BS2JP2_rEE, prop_choice_between_BS2JP2[4]-prop_choice_between_BS2JP2[3])# f vulnérable - f robuste
      TSREE_between_BS2JP2_rEE <- append(TSREE_between_BS2JP2_rEE, 1 + prop_choice_between_BS2JP2[1]-prop_choice_between_BS2JP2[2])# 1 + f antifragile - f fragile)
      OSSREE_after_JP2_rEE <- append(OSSREE_after_JP2_rEE, prop_choice_after_JP2[4]-prop_choice_after_JP2[3]) # f vulnérable - f robuste
      TSREE_after_JP2_rEE <- append(TSREE_after_JP2_rEE, 1 + prop_choice_after_JP2[1]-prop_choice_after_JP2[2]) # 1 + f antifragile - f fragile)
    }
    # JP1 BS1 JP2 BS2 
    if(apparition_BS[1]>apparition_JP[1]){
      llabels_JP1BS2JP2BS2 <- append(llabels_JP1BS2JP2BS2, i)
      before_JP1 = four_choice_i[1:apparition_JP[1]]
      between_JP1BS1 = four_choice_i[apparition_JP[1]:apparition_BS[1]]
      between_BS1JP2 = four_choice_i[apparition_BS[1]:apparition_JP[2]]
      between_JP2BS2 = four_choice_i[apparition_JP[2]:apparition_BS[2]]
      after_BS2 = four_choice_i[apparition_BS[2]:n]
      print(table(before_JP1)/length(before_JP1))
      prop_choice_before_JP1 = table(before_JP1)/length(before_JP1)
      print(table(between_JP1BS1)/length(between_JP1BS1))
      prop_choice_between_JP1BS1 = table(between_JP1BS1)/length(between_JP1BS1)
      print(table(between_BS1JP2)/length(between_BS1JP2))
      prop_choice_between_BS1JP2 = table(between_BS1JP2)/length(between_BS1JP2)
      print(table(between_JP2BS2)/length(between_JP2BS2))
      prop_choice_between_JP2BS2 = table(between_JP2BS2)/length(between_JP2BS2)
      print(table(after_BS2)/length(after_BS2))
      prop_choice_after_BS2 = table(after_BS2)/length(after_BS2)
      OSSREE_before_JP1_rEE <- append(OSSREE_before_JP1_rEE, prop_choice_before_JP1[4]-prop_choice_before_JP1[3]) # f vulnérable - f robuste
      TSREE_before_JP1_rEE <- append(TSREE_before_JP1_rEE, 1 + prop_choice_before_JP1[1]-prop_choice_before_JP1[2]) # 1 + f antifragile - f fragile)
      OSSREE_between_JP1BS1_rEE <- append(OSSREE_between_JP1BS1_rEE, prop_choice_between_JP1BS1[4]-prop_choice_between_JP1BS1[3])# f vulnérable - f robuste
      TSREE_between_JP1BS1_rEE <- append(TSREE_between_JP1BS1_rEE, 1 + prop_choice_between_JP1BS1[1]-prop_choice_between_JP1BS1[2])# 1 + f antifragile - f fragile)
      OSSREE_between_BS1JP2_rEE <- append(OSSREE_between_BS1JP2_rEE, prop_choice_between_BS1JP2[4]-prop_choice_between_BS1JP2[3])# f vulnérable - f robuste
      TSREE_between_BS1JP2_rEE <- append(TSREE_between_BS1JP2_rEE, 1 + prop_choice_between_BS1JP2[1]-prop_choice_between_BS1JP2[2])# 1 + f antifragile - f fragile)
      OSSREE_between_JP2BS2_rEE <- append(OSSREE_between_JP2BS2_rEE, prop_choice_between_JP2BS2[4]-prop_choice_between_JP2BS2[3])# f vulnérable - f robuste
      TSREE_between_JP2BS2_rEE <- append(TSREE_between_JP2BS2_rEE, 1 + prop_choice_between_JP2BS2[1]-prop_choice_between_JP2BS2[2])# 1 + f antifragile - f fragile)
      OSSREE_after_BS2_rEE <- append(OSSREE_after_BS2_rEE, prop_choice_after_BS2[4]-prop_choice_after_BS2[3]) # f vulnérable - f robuste
      TSREE_after_BS2_rEE <- append(TSREE_after_BS2_rEE, 1 + prop_choice_after_BS2[1]-prop_choice_after_BS2[2]) # 1 + f antifragile - f fragile)
    }

  }
  
  # si 3 JP et 2 BS
  if (length(apparition_BS)==2 && length(apparition_JP) == 3){
    # ici c'est donc forcément JP1 BS1 JP2 BS2 JP3
    llabels_JP1BS1JP2BS2JP3 <- append(llabels_JP1BS1JP2BS2JP3, i)
    before_JP1 = four_choice_i[1:apparition_JP[1]]
    between_JP1BS1 = four_choice_i[apparition_JP[1]:apparition_BS[1]]
    between_BS1JP2 = four_choice_i[apparition_BS[1]:apparition_JP[2]]
    between_JP2BS2 = four_choice_i[apparition_JP[2]:apparition_BS[2]]
    between_BS2JP3 = four_choice_i[apparition_BS[2]:apparition_JP[3]]
    after_JP3 = four_choice_i[apparition_JP[3]:n]
    print(table(before_JP1)/length(before_JP1))
    prop_choice_before_JP1 = table(before_JP1)/length(before_JP1)
    print(table(between_JP1BS1)/length(between_JP1BS1))
    prop_choice_between_JP1BS1 = table(between_JP1BS1)/length(between_JP1BS1)
    print(table(between_BS1JP2)/length(between_BS1JP2))
    prop_choice_between_BS1JP2 = table(between_BS1JP2)/length(between_BS1JP2)
    print(table(between_JP2BS2)/length(between_JP2BS2))
    prop_choice_between_JP2BS2 = table(between_JP2BS2)/length(between_JP2BS2)
    print(table(between_BS2JP3)/length(between_BS2JP3))
    prop_choice_between_BS2JP3 = table(between_BS2JP3)/length(between_BS2JP3)
    print(table(after_JP3)/length(after_JP3))
    prop_choice_after_JP3 = table(after_JP3)/length(after_JP3)
    
    OSSREE_before_JP1_REE <- append(OSSREE_before_JP1_REE, prop_choice_before_JP1[4]-prop_choice_before_JP1[3]) # f vulnérable - f robuste
    TSREE_before_JP1_REE <- append(TSREE_before_JP1_REE, 1 + prop_choice_before_JP1[1]-prop_choice_before_JP1[2]) # 1 + f antifragile - f fragile)
    OSSREE_between_JP1BS1_REE <- append(OSSREE_between_JP1BS1_REE, prop_choice_between_JP1BS1[4]-prop_choice_between_JP1BS1[3])# f vulnérable - f robuste
    TSREE_between_JP1BS1_REE <- append(TSREE_between_JP1BS1_REE, 1 + prop_choice_between_JP1BS1[1]-prop_choice_between_JP1BS1[2])# 1 + f antifragile - f fragile)
    OSSREE_between_BS1JP2_REE <- append(OSSREE_between_BS1JP2_REE, prop_choice_between_BS1JP2[4]-prop_choice_between_BS1JP2[3])# f vulnérable - f robuste
    TSREE_between_BS1JP2_REE <- append(TSREE_between_BS1JP2_REE, 1 + prop_choice_between_BS1JP2[1]-prop_choice_between_BS1JP2[2])# 1 + f antifragile - f fragile)
    OSSREE_between_JP2BS2_REE <- append(OSSREE_between_JP2BS2_REE, prop_choice_between_JP2BS2[4]-prop_choice_between_JP2BS2[3])# f vulnérable - f robuste
    TSREE_between_JP2BS2_REE <- append(TSREE_between_JP2BS2_REE, 1 + prop_choice_between_JP2BS2[1]-prop_choice_between_JP2BS2[2])# 1 + f antifragile - f fragile)
    OSSREE_between_BS2JP3_REE <- append(OSSREE_between_BS2JP3_REE, prop_choice_between_BS2JP3[4]-prop_choice_between_BS2JP3[3])# f vulnérable - f robuste
    TSREE_between_BS2JP3_REE <- append(TSREE_between_BS2JP3_REE, 1 + prop_choice_between_BS2JP3[1]-prop_choice_between_BS2JP3[2])# 1 + f antifragile - f fragile)
    OSSREE_after_JP3_REE <- append(OSSREE_after_JP3_REE, prop_choice_after_JP3[4]-prop_choice_after_JP3[3]) # f vulnérable - f robuste
    TSREE_after_JP3_REE <- append(TSREE_after_JP3_REE, 1 + prop_choice_after_JP3[1]-prop_choice_after_JP3[2]) # 1 + f antifragile - f fragile)

  }
  
  # si 3 BS et 2 JP 
  if (length(apparition_BS)==3 && length(apparition_JP) == 2){
    # ici c'est donc forcément BS1 JP1 BS2 JP2 BS3
    llabels_BS1JP1BS2JP3BS3 <- append(llabels_BS1JP1BS2JP3BS3, i)
    before_BS1 = four_choice_i[1:apparition_BS[1]]
    between_BS1JP1 = four_choice_i[apparition_BS[1]:apparition_JP[1]]
    between_JP1BS2 = four_choice_i[apparition_JP[1]:apparition_BS[2]]
    between_BS2JP2 = four_choice_i[apparition_BS[2]:apparition_JP[2]]
    between_JP2BS3 = four_choice_i[apparition_JP[2]:apparition_BS[3]]
    after_BS3 = four_choice_i[apparition_BS[3]:n]
    print(table(before_BS1)/length(before_BS1))
    prop_choice_before_BS1 = table(before_BS1)/length(before_BS1)
    print(table(between_BS1JP1)/length(between_BS1JP1))
    prop_choice_between_BS1JP1 = table(between_BS1JP1)/length(between_BS1JP1)
    print(table(between_JP1BS2)/length(between_JP1BS2))
    prop_choice_between_JP1BS2 = table(between_JP1BS2)/length(between_JP1BS2)
    print(table(between_BS2JP2)/length(between_BS2JP2))
    prop_choice_between_BS2JP2 = table(between_BS2JP2)/length(between_BS2JP2)
    print(table(between_JP2BS3)/length(between_JP2BS3))
    prop_choice_between_JP2BS3 = table(between_JP2BS3)/length(between_JP2BS3)
    print(table(after_BS3)/length(after_BS3))
    prop_choice_after_BS3 = table(after_BS3)/length(after_BS3)
    
    OSSREE_before_BS1_REE <- append(OSSREE_before_BS1_REE, prop_choice_before_BS1[4]-prop_choice_before_BS1[3]) # f vulnérable - f robuste
    TSREE_before_BS1_REE <- append(TSREE_before_BS1_REE, 1 + prop_choice_before_BS1[1]-prop_choice_before_BS1[2]) # 1 + f antifragile - f fragile)
    OSSREE_between_BS1JP1_REE <- append(OSSREE_between_BS1JP1_REE, prop_choice_between_BS1JP1[4]-prop_choice_between_BS1JP1[3])# f vulnérable - f robuste
    TSREE_between_BS1JP1_REE <- append(TSREE_between_BS1JP1_REE, 1 + prop_choice_between_BS1JP1[1]-prop_choice_between_BS1JP1[2])# 1 + f antifragile - f fragile)
    OSSREE_between_JP1BS2_REE <- append(OSSREE_between_JP1BS2_REE, prop_choice_between_JP1BS2[4]-prop_choice_between_JP1BS2[3])# f vulnérable - f robuste
    TSREE_between_JP1BS2_REE <- append(TSREE_between_JP1BS2_REE, 1 + prop_choice_between_JP1BS2[1]-prop_choice_between_JP1BS2[2])# 1 + f antifragile - f fragile)
    OSSREE_between_BS2JP2_REE <- append(OSSREE_between_BS2JP2_REE, prop_choice_between_BS2JP2[4]-prop_choice_between_BS2JP2[3])# f vulnérable - f robuste
    TSREE_between_BS2JP2_REE <- append(TSREE_between_BS2JP2_REE, 1 + prop_choice_between_BS2JP2[1]-prop_choice_between_BS2JP2[2])# 1 + f antifragile - f fragile)
    OSSREE_between_JP2BS3_REE <- append(OSSREE_between_JP2BS3_REE, prop_choice_between_JP2BS3[4]-prop_choice_between_JP2BS3[3])# f vulnérable - f robuste
    TSREE_between_JP2BS3_REE <- append(TSREE_between_JP2BS3_REE, 1 + prop_choice_between_JP2BS3[1]-prop_choice_between_JP2BS3[2])# 1 + f antifragile - f fragile)
    OSSREE_after_BS3_REE <- append(OSSREE_after_BS3_REE, prop_choice_after_BS3[4]-prop_choice_after_BS3[3]) # f vulnérable - f robuste
    TSREE_after_BS3_REE <- append(TSREE_after_BS3_REE, 1 + prop_choice_after_BS3[1]-prop_choice_after_BS3[2]) # 1 + f antifragile - f fragile)

  }
  
  # si 3 JP et 1 BS
  if (length(apparition_JP)==3 && length(apparition_BS) == 1 && apparition_BS[1] != 1){
    # deux solution dans ce cas : 
    # JP1 BS JP2 JP3 ou bien 
    # JP1 JP2 BS JP3
    
    # JP1 BS JP2 JP3
    if (apparition_JP[2] > apparition_BS[1]){
      llabels_JP1BSJP2JP3 <- append(llabels_JP1BSJP2JP3, i)
      before_JP1 = four_choice_i[1:apparition_JP[1]]
      between_JP1BS = four_choice_i[apparition_JP[1]:apparition_BS[1]]
      between_BSJP2 = four_choice_i[apparition_BS[1]:apparition_JP[2]]
      between_JP2JP3 = four_choice_i[apparition_JP[2]:apparition_JP[3]]
      after_JP3 = four_choice_i[apparition_JP[3]:n]
      print(table(before_JP1)/length(before_JP1))
      prop_choice_before_JP1 = table(before_JP1)/length(before_JP1)
      print(table(between_JP1BS)/length(between_JP1BS))
      prop_choice_between_JP1BS = table(between_JP1BS)/length(between_JP1BS)
      print(table(between_BSJP2)/length(between_BSJP2))
      prop_choice_between_BSJP2 = table(between_BSJP2)/length(between_BSJP2)
      print(table(between_JP2JP3)/length(between_JP2JP3))
      prop_choice_between_JP2JP3 = table(between_JP2JP3)/length(between_JP2JP3)
      print(table(after_JP3)/length(after_JP3))
      prop_choice_after_JP3 = table(after_JP3)/length(after_JP3)
      OSSREE_before_JP1_REEe <- append(OSSREE_before_JP1_REEe, prop_choice_before_JP1[4]-prop_choice_before_JP1[3]) # f vulnérable - f robuste
      TSREE_before_JP1_REEe <- append(TSREE_before_JP1_REEe, 1 + prop_choice_before_JP1[1]-prop_choice_before_JP1[2]) # 1 + f antifragile - f fragile)
      OSSREE_between_JP1BS_REEe <- append(OSSREE_between_JP1BS_REEe, prop_choice_between_JP1BS[4]-prop_choice_between_JP1BS[3])# f vulnérable - f robuste
      TSREE_between_JP1BS_REEe <- append(TSREE_between_JP1BS_REEe, 1 + prop_choice_between_JP1BS[1]-prop_choice_between_JP1BS[2])# 1 + f antifragile - f fragile)
      OSSREE_between_BSJP2_REEe <- append(OSSREE_between_BSJP2_REEe, prop_choice_between_BSJP2[4]-prop_choice_between_BSJP2[3])# f vulnérable - f robuste
      TSREE_between_BSJP2_REEe <- append(TSREE_between_BSJP2_REEe, 1 + prop_choice_between_BSJP2[1]-prop_choice_between_BSJP2[2])# 1 + f antifragile - f fragile)
      OSSREE_between_JP2JP3_REEe <- append(OSSREE_between_JP2JP3_REEe, prop_choice_between_JP2JP3[4]-prop_choice_between_JP2JP3[3])# f vulnérable - f robuste
      TSREE_between_JP2JP3_REEe <- append(TSREE_between_JP2JP3_REEe, 1 + prop_choice_between_JP2JP3[1]-prop_choice_between_JP2JP3[2])# 1 + f antifragile - f fragile)
      OSSREE_after_JP3_REEe <- append(OSSREE_after_JP3_REEe, prop_choice_after_JP3[4]-prop_choice_after_JP3[3]) # f vulnérable - f robuste
      TSREE_after_JP3_REEe <- append(TSREE_after_JP3_REEe, 1 + prop_choice_after_JP3[1]-prop_choice_after_JP3[2]) # 1 + f antifragile - f fragile)
      
    }
    # JP1 JP2 BS JP3
    if(apparition_JP[2] < apparition_BS[1]){
      llabels_JP1JP2BSJP3 <- append(llabels_JP1JP2BSJP3, i)
      before_JP1 = four_choice_i[1:apparition_JP[1]]
      between_JP1JP2 = four_choice_i[apparition_JP[1]:apparition_JP[2]]
      between_JP2BS = four_choice_i[apparition_JP[2]:apparition_BS[1]]
      between_BSJP3 = four_choice_i[apparition_BS[1]:apparition_JP[3]]
      after_JP3 = four_choice_i[apparition_JP[3]:n]
      print(table(before_JP1)/length(before_JP1))
      prop_choice_before_JP1 = table(before_JP1)/length(before_JP1)
      print(table(between_JP1JP2)/length(between_JP1JP2))
      prop_choice_between_JP1JP2 = table(between_JP1JP2)/length(between_JP1JP2)
      print(table(between_JP2BS)/length(between_JP2BS))
      prop_choice_between_JP2BS = table(between_JP2BS)/length(between_JP2BS)
      print(table(between_BSJP3)/length(between_BSJP3))
      prop_choice_between_BSJP3 = table(between_BSJP3)/length(between_BSJP3)
      print(table(after_JP2)/length(after_JP2))
      prop_choice_after_JP3 = table(after_JP3)/length(after_JP2)
      OSSREE_before_JP1_REEE <- append(OSSREE_before_JP1_REEE, prop_choice_before_JP1[4]-prop_choice_before_JP1[3]) # f vulnérable - f robuste
      TSREE_before_JP1_REEE <- append(TSREE_before_JP1_REEE, 1 + prop_choice_before_JP1[1]-prop_choice_before_JP1[2]) # 1 + f antifragile - f fragile)
      OSSREE_between_JP1JP2_REEE <- append(OSSREE_between_JP1JP2_REEE, prop_choice_between_JP1JP2[4]-prop_choice_between_JP1JP2[3])# f vulnérable - f robuste
      TSREE_between_JP1JP2_REEE <- append(TSREE_between_JP1JP2_REEE, 1 + prop_choice_between_JP1JP2[1]-prop_choice_between_JP1JP2[2])# 1 + f antifragile - f fragile)
      OSSREE_between_JP2BS_REEE <- append(OSSREE_between_JP2BS_REEE, prop_choice_between_JP2BS[4]-prop_choice_between_JP2BS[3])# f vulnérable - f robuste
      TSREE_between_JP2BS_REEE <- append(TSREE_between_JP2BS_REEE, 1 + prop_choice_between_JP2BS[1]-prop_choice_between_JP2BS[2])# 1 + f antifragile - f fragile)
      OSSREE_between_BSJP3_REEE <- append(OSSREE_between_BSJP3_REEE, prop_choice_between_BSJP3[4]-prop_choice_between_BSJP3[3])# f vulnérable - f robuste
      TSREE_between_BSJP3_REEE <- append(TSREE_between_BSJP3_REEE, 1 + prop_choice_between_BSJP3[1]-prop_choice_between_BSJP3[2])# 1 + f antifragile - f fragile)
      OSSREE_after_JP3_REEE <- append(OSSREE_after_JP3_REEE, prop_choice_after_JP3[4]-prop_choice_after_JP3[3]) # f vulnérable - f robuste
      TSREE_after_JP3_REEE <- append(TSREE_after_JP3_REEE, 1 + prop_choice_after_JP3[1]-prop_choice_after_JP3[2]) # 1 + f antifragile - f fragile)
    }
  }
  
  # si 3 BS et 1 JP 
  if (length(apparition_BS)==3 && length(apparition_JP) == 1 && apparition_JP[1]!= 100){
    # deux possibilité dans ce cas
    # BS1 JP BS2 BS3 ou bien 
    # BS1 BS2 JP BS3
    
    # BS1 JP BS2 BS3
    if (apparition_JP[1] < apparition_BS[2]){
      llabels_BS1JPBS2BS3 <- append(llabels_BS1JPBS2BS3, i)
      before_BS1 = four_choice_i[1:apparition_BS[1]]
      between_BS1JP = four_choice_i[apparition_BS[1]:apparition_JP[1]]
      between_JPBS2 = four_choice_i[apparition_JP[1]:apparition_BS[2]]
      between_BS2BS3 = four_choice_i[apparition_BS[2]:apparition_BS[3]]
      after_BS3 = four_choice_i[apparition_BS[3]:n]
      print(table(before_BS1)/length(before_BS1))
      prop_choice_before_BS1 = table(before_BS1)/length(before_BS1)
      print(table(between_BS1JP)/length(between_BS1JP))
      prop_choice_between_BS1JP = table(between_BS1JP)/length(between_BS1JP)
      print(table(between_JPBS2)/length(between_JPBS2))
      prop_choice_between_JPBS2 = table(between_JPBS2)/length(between_JPBS2)
      print(table(between_BS2BS3)/length(between_BS2BS3))
      prop_choice_between_BS2BS3 = table(between_BS2BS3)/length(between_BS2BS3)
      print(table(after_BS3)/length(after_BS3))
      prop_choice_after_BS3 = table(after_BS3)/length(after_BS3)
      OSSREE_before_BS1_REEe <- append(OSSREE_before_BS1_REEe, prop_choice_before_BS1[4]-prop_choice_before_BS1[3]) # f vulnérable - f robuste
      TSREE_before_BS1_REEe <- append(TSREE_before_BS1_REEe, 1 + prop_choice_before_BS1[1]-prop_choice_before_BS1[2]) # 1 + f antifragile - f fragile)
      OSSREE_between_BS1JP_REEe <- append(OSSREE_between_BS1JP_REEe, prop_choice_between_BS1JP[4]-prop_choice_between_BS1JP[3])# f vulnérable - f robuste
      TSREE_between_BS1JP_REEe <- append(TSREE_between_BS1JP_REEe, 1 + prop_choice_between_BS1JP[1]-prop_choice_between_BS1JP[2])# 1 + f antifragile - f fragile)
      OSSREE_between_JPBS2_REEe <- append(OSSREE_between_JPBS2_REEe, prop_choice_between_JPBS2[4]-prop_choice_between_JPBS2[3])# f vulnérable - f robuste
      TSREE_between_JPBS2_REEe <- append(TSREE_between_JPBS2_REEe, 1 + prop_choice_between_JPBS2[1]-prop_choice_between_JPBS2[2])# 1 + f antifragile - f fragile)
      OSSREE_between_BS2BS3_REEe <- append(OSSREE_between_BS2BS3_REEe, prop_choice_between_BS2BS3[4]-prop_choice_between_BS2BS3[3])# f vulnérable - f robuste
      TSREE_between_BS2BS3_REEe <- append(TSREE_between_BS2BS3_REEe, 1 + prop_choice_between_BS2BS3[1]-prop_choice_between_BS2BS3[2])# 1 + f antifragile - f fragile)
      OSSREE_after_BS3_REEe <- append(OSSREE_after_BS3_REEe, prop_choice_after_BS3[4]-prop_choice_after_BS3[3]) # f vulnérable - f robuste
      TSREE_after_BS3_REEe <- append(TSREE_after_BS3_REEe, 1 + prop_choice_after_BS3[1]-prop_choice_after_BS3[2]) # 1 + f antifragile - f fragile)

    }
    # BS1 BS2 JP BS3
    if(apparition_JP[1] > apparition_BS[2]){
      llabels_BS1BS2JPBS3 <- append(llabels_BS1BS2JPBS3, i)
      before_BS1 = four_choice_i[1:apparition_BS[1]]
      between_BS1BS2 = four_choice_i[apparition_BS[1]:apparition_BS[2]]
      between_BS2JP = four_choice_i[apparition_BS[2]:apparition_JP[1]]
      between_JPBS3 = four_choice_i[apparition_JP[1]:apparition_BS[3]]
      after_BS3 = four_choice_i[apparition_BS[3]:n]
      print(table(before_BS1)/length(before_BS1))
      prop_choice_before_BS1 = table(before_BS1)/length(before_BS1)
      print(table(between_BS1BS2)/length(between_BS1BS2))
      prop_choice_between_BS1BS2 = table(between_BS1BS2)/length(between_BS1BS2)
      print(table(between_BS2JP)/length(between_BS2JP))
      prop_choice_between_BS2JP = table(between_BS2JP)/length(between_BS2JP)
      print(table(between_JPBS3)/length(between_JPBS3))
      prop_choice_between_JPBS3 = table(between_JPBS3)/length(between_JPBS3)
      print(table(after_BS3)/length(after_BS3))
      prop_choice_after_BS3 = table(after_BS3)/length(after_BS3)
      OSSREE_before_BS1_REEE <- append(OSSREE_before_BS1_REEE, prop_choice_before_BS1[4]-prop_choice_before_BS1[3]) # f vulnérable - f robuste
      TSREE_before_BS1_REEE <- append(TSREE_before_BS1_REEE, 1 + prop_choice_before_BS1[1]-prop_choice_before_BS1[2]) # 1 + f antifragile - f fragile)
      OSSREE_between_BS1BS2_REEE <- append(OSSREE_between_BS1BS2_REEE, prop_choice_between_BS1BS2[4]-prop_choice_between_BS1BS2[3])# f vulnérable - f robuste
      TSREE_between_BS1BS2_REEE <- append(TSREE_between_BS1BS2_REEE, 1 + prop_choice_between_BS1BS2[1]-prop_choice_between_BS1BS2[2])# 1 + f antifragile - f fragile)
      OSSREE_between_BS2JP_REEE <- append(OSSREE_between_BS2JP_REEE, prop_choice_between_BS2JP[4]-prop_choice_between_BS2JP[3])# f vulnérable - f robuste
      TSREE_between_BS2JP_REEE <- append(TSREE_between_BS2JP_REEE, 1 + prop_choice_between_BS2JP[1]-prop_choice_between_BS2JP[2])# 1 + f antifragile - f fragile)
      OSSREE_between_JPBS3_REEE <- append(OSSREE_between_JPBS3_REEE, prop_choice_between_JPBS3[4]-prop_choice_between_JPBS3[3])# f vulnérable - f robuste
      TSREE_between_JPBS3_REEE <- append(TSREE_between_JPBS3_REEE, 1 + prop_choice_between_JPBS3[1]-prop_choice_between_JPBS3[2])# 1 + f antifragile - f fragile)
      OSSREE_after_BS3_REEE <- append(OSSREE_after_BS3_REEE, prop_choice_after_BS3[4]-prop_choice_after_BS3[3]) # f vulnérable - f robuste
      TSREE_after_BS3_REEE <- append(TSREE_after_BS3_REEE, 1 + prop_choice_after_BS3[1]-prop_choice_after_BS3[2]) # 1 + f antifragile - f fragile)
      
    }
  }
  
  # auncun REE ni JP ni BS
  if (apparition_JP[1] == 100 && apparition_BS[1] == 1){ # pas JP ni BS
    llabels_none <- append(llabels_none, i)
    print(table(four_choice_i)/n)
    prop_choice = table(four_choice_i)/n
    OSSREE_noneREE <- append(OSSREE_noneREE, prop_choice[4]-prop_choice[3]) # f vulnérable - f robuste)
    TSREE_noneREE <- append(TSREE_noneREE, 1 + prop_choice[1]-prop_choice[2]) # 1 + f antifragile - f fragile)
    
  }
  
}

## UNLIST the list of list 
# the ones that only experienced only one JP
OSSREE_before_JP = unlist(OSSREE_before_JP)
TSREE_before_JP = unlist(TSREE_before_JP)
OSSREE_after_JP = unlist(OSSREE_after_JP)
TSREE_after_JP = unlist(TSREE_after_JP)
llabels_JP = unlist(llabels_JP)
# the ones that only experienced 2 JPs
OSSREE_before_JP1 = unlist(OSSREE_before_JP1)
TSREE_before_JP1 = unlist(TSREE_before_JP1)
OSSREE_between_JPs = unlist(OSSREE_between_JPs)
TSREE_between_JPs = unlist(TSREE_between_JPs)
OSSREE_after_JP2 = unlist(OSSREE_after_JP2)
TSREE_after_JP2 = unlist(TSREE_after_JP2)
llabels_2JP = unlist(llabels_2JP)
# the ones that only experienced 3 JPs
OSSREE_before_JP1s = unlist(OSSREE_before_JP1s)
TSREE_before_JP1s = unlist(TSREE_before_JP1s)
OSSREE_between_JP1_JP2 = unlist(OSSREE_between_JP1_JP2)
TSREE_between_JP1_JP2 = unlist(TSREE_between_JP1_JP2)
OSSREE_between_JP2_JP3 = unlist(OSSREE_between_JP2_JP3)
TSREE_between_JP2_JP3 = unlist(TSREE_between_JP2_JP3)
OSSREE_after_JP3 = unlist(OSSREE_after_JP3)
TSREE_after_JP3 = unlist(TSREE_after_JP3)
llabels_3JP = unlist(llabels_3JP)

#the ones that only experienced only one BS
OSSREE_before_BS = unlist(OSSREE_before_BS)
TSREE_before_BS = unlist(TSREE_before_BS)
OSSREE_after_BS = unlist(OSSREE_after_BS)
TSREE_after_BS = unlist(TSREE_after_BS)
llabels_BS = unlist(llabels_BS)
# the ones that only experienced 2 BSs
OSSREE_before_BS1 = unlist(OSSREE_before_BS1)
TSREE_before_BS1 = unlist(TSREE_before_BS1)
OSSREE_between_BSs = unlist(OSSREE_between_BSs)
TSREE_between_BSs = unlist(TSREE_between_BSs)
OSSREE_after_BS2 = unlist(OSSREE_after_BS2)
TSREE_after_BS2 = unlist(TSREE_after_BS2)
llabels_2BS = unlist(llabels_2BS)
# the ones that only experienced 3 BSs
OSSREE_before_BS1s = unlist(OSSREE_before_BS1s)
TSREE_before_BS1s = unlist(TSREE_before_BS1s)
OSSREE_between_BS1_BS2 = unlist(OSSREE_between_BS1_BS2)
TSREE_between_BS1_BS2 = unlist(TSREE_between_BS1_BS2)
OSSREE_between_BS2_BS3 = unlist(OSSREE_between_BS2_BS3)
TSREE_between_BS2_BS3 = unlist(TSREE_between_BS2_BS3)
OSSREE_after_BS3 = unlist(OSSREE_after_BS3)
TSREE_after_BS3 = unlist(TSREE_after_BS3)
llabels_3BS = unlist(llabels_3BS)

## the ones that experienced exactly 1 JP and 1 BS
# BS then JP
OSSREE_before_BS_BSJP = unlist(OSSREE_before_BS_BSJP)
TSREE_before_BS_BSJP = unlist(TSREE_before_BS_BSJP)
OSSREE_between_BSJP = unlist(OSSREE_between_BSJP)
TSREE_between_BSJP = unlist(TSREE_between_BSJP)
OSSREE_after_JP_BSJP = unlist(OSSREE_after_JP_BSJP)
TSREE_after_JP_BSJP = unlist(TSREE_after_JP_BSJP)
llabels_BSJP = unlist(llabels_BSJP)
# JP then BS 
OSSREE_before_JP_JPBS = unlist(OSSREE_before_JP_JPBS)
TSREE_before_JP_JPBS = unlist(TSREE_before_JP_JPBS)
OSSREE_between_JPBS = unlist(OSSREE_between_JPBS)
TSREE_between_JPBS = unlist(TSREE_between_JPBS)
OSSREE_after_BS_JPBS = unlist(OSSREE_after_BS_JPBS)
TSREE_after_BS_JPBS = unlist(TSREE_after_BS_JPBS)
llabels_JPBS = unlist(llabels_JPBS)

## the ones that experienced 2 JP and 1 BS
# BS1 JP1 JP2
OSSREE_before_BS1_Ree = unlist(OSSREE_before_BS1_Ree)
TSREE_before_BS1_Ree = unlist(TSREE_before_BS1_Ree)
OSSREE_between_BS1JP1_Ree = unlist(OSSREE_between_BS1JP1_Ree)
TSREE_between_BS1JP1_Ree = unlist(TSREE_between_BS1JP1_Ree)
OSSREE_between_JP1JP2_Ree = unlist(OSSREE_between_JP1JP2_Ree)
TSREE_between_JP1JP2_Ree = unlist(TSREE_between_JP1JP2_Ree)
OSSREE_after_JP2_Ree = unlist(OSSREE_after_JP2_Ree)
TSREE_after_JP2_Ree = unlist(TSREE_after_JP2_Ree)
llabels_BS1JP1JP2 = unlist(llabels_BS1JP1JP2)
# JP1 BS2 JP2
OSSREE_before_JP1_reE = unlist(OSSREE_before_JP1_reE)
TSREE_before_JP1_reE = unlist(TSREE_before_JP1_reE)
OSSREE_between_JP1BS2_reE = unlist(OSSREE_between_JP1BS2_reE)
TSREE_between_JP1BS2_reE = unlist(TSREE_between_JP1BS2_reE)
OSSREE_between_BS2JP2_reE = unlist(OSSREE_between_BS2JP2_reE)
TSREE_between_BS2JP2_reE = unlist(TSREE_between_BS2JP2_reE)
OSSREE_after_JP2_reE = unlist(OSSREE_after_JP2_reE)
TSREE_after_JP2_reE = unlist(TSREE_after_JP2_reE)
llabels_JP1BS2JP2 = unlist(llabels_JP1BS2JP2)
# JP1 JP2 BS
OSSREE_before_JP1_reEE = unlist(OSSREE_before_JP1_reEE)
TSREE_before_JP1_reEE = unlist(TSREE_before_JP1_reEE)
OSSREE_between_JP1JP2_reEE = unlist(OSSREE_between_JP1JP2_reEE)
TSREE_between_JP1JP2_reEE = unlist(TSREE_between_JP1JP2_reEE)
OSSREE_between_JP2BS_reEE = unlist(OSSREE_between_JP2BS_reEE)
TSREE_between_JP2BS_reEE = unlist(TSREE_between_JP2BS_reEE)
OSSREE_after_BS_reEE = unlist(OSSREE_after_BS_reEE)
TSREE_after_BS_reEE = unlist(TSREE_after_BS_reEE)
llabels_JP1JP2BS = unlist(llabels_JP1JP2BS)

## the ones that experienced 2 BS and 1 JP 
# BS1 JP1 BS2
OSSREE_before_BS1_REe = unlist(OSSREE_before_BS1_REe)
TSREE_before_BS1_REe = unlist(TSREE_before_BS1_REe)
OSSREE_between_BS1JP1_REe = unlist(OSSREE_between_BS1JP1_REe)
TSREE_between_BS1JP1_REe = unlist(TSREE_between_BS1JP1_REe)
OSSREE_between_JP1BS2_REe = unlist(OSSREE_between_JP1BS2_REe)
TSREE_between_JP1BS2_REe = unlist(TSREE_between_JP1BS2_REe)
OSSREE_after_BS2_REe = unlist(OSSREE_after_BS2_REe)
TSREE_after_BS2_REe = unlist(TSREE_after_BS2_REe)
llabels_BS1JP1BS2 = unlist(llabels_BS1JP1BS2)
# BS1 BS2 JP2
OSSREE_before_BS1_ree = unlist(OSSREE_before_BS1_ree)
TSREE_before_BS1_ree = unlist(TSREE_before_BS1_ree)
OSSREE_between_BS1BS2_ree = unlist(OSSREE_between_BS1BS2_ree)
TSREE_between_BS1BS2_ree = unlist(TSREE_between_BS1BS2_ree)
OSSREE_between_BS2JP2_ree = unlist(OSSREE_between_BS2JP2_ree)
TSREE_between_BS2JP2_ree = unlist(TSREE_between_BS2JP2_ree)
OSSREE_after_JP2_ree = unlist(OSSREE_after_JP2_ree)
TSREE_after_JP2_ree = unlist(TSREE_after_JP2_ree)
llabels_BS1BS2JP2 = unlist(llabels_BS1BS2JP2)
# JP BS1 BS2
OSSREE_before_JP_reee = unlist(OSSREE_before_JP_reee)
TSREE_before_JP_reee = unlist(TSREE_before_JP_reee)
OSSREE_between_JPBS1_reee = unlist(OSSREE_between_JPBS1_reee)
TSREE_between_JPBS1_reee = unlist(TSREE_between_JPBS1_reee)
OSSREE_between_BS1BS2_reee = unlist(OSSREE_between_BS1BS2_reee)
TSREE_between_BS1BS2_reee = unlist(TSREE_between_BS1BS2_reee)
OSSREE_after_BS2_reee = unlist(OSSREE_after_BS2_reee)
TSREE_after_BS2_reee = unlist(TSREE_after_BS2_reee)
llabels_JPBS1BS2 = unlist(llabels_JPBS1BS2)

## the ones experienced 2 JP and 2 BS
# BS1 JP1 BS2 JP2
OSSREE_before_BS1_rEE = unlist(OSSREE_before_BS1_rEE)
TSREE_before_BS1_rEE = unlist(TSREE_before_BS1_rEE)
OSSREE_between_BS1JP1_rEE = unlist(OSSREE_between_BS1JP1_rEE)
TSREE_between_BS1JP1_rEE = unlist(TSREE_between_BS1JP1_rEE)
OSSREE_between_JP1BS2_rEE = unlist(OSSREE_between_JP1BS2_rEE)
TSREE_between_JP1BS2_rEE = unlist(TSREE_between_JP1BS2_rEE)
OSSREE_between_BS2JP2_rEE = unlist(OSSREE_between_BS2JP2_rEE)
TSREE_between_BS2JP2_rEE = unlist(TSREE_between_BS2JP2_rEE)
OSSREE_after_JP2_rEE = unlist(OSSREE_after_JP2_rEE)
TSREE_after_JP2_rEE = unlist(TSREE_after_JP2_rEE)
llabels_BS1JP1BS2JP2 = unlist(llabels_BS1JP1BS2JP2)
# JP1 BS1 JP2 BS2 
OSSREE_before_JP1_rEE = unlist(OSSREE_before_JP1_rEE)
TSREE_before_JP1_rEE = unlist(TSREE_before_JP1_rEE)
OSSREE_between_JP1BS1_rEE = unlist(OSSREE_between_JP1BS1_rEE)
TSREE_between_JP1BS1_rEE = unlist(TSREE_between_JP1BS1_rEE)
OSSREE_between_BS1JP2_rEE = unlist(OSSREE_between_BS1JP2_rEE)
TSREE_between_BS1JP2_rEE = unlist(TSREE_between_BS1JP2_rEE)
OSSREE_between_JP2BS2_rEE = unlist(OSSREE_between_JP2BS2_rEE)
TSREE_between_JP2BS2_rEE = unlist(TSREE_between_JP2BS2_rEE)
OSSREE_after_BS2_rEE = unlist(OSSREE_after_BS2_rEE)
TSREE_after_BS2_rEE = unlist(TSREE_after_BS2_rEE)
llabels_JP1BS2JP2BS2 = unlist(llabels_JP1BS2JP2BS2)

## the ones experienced 3 JP and 2 BS
OSSREE_before_JP1_REE = unlist(OSSREE_before_JP1_REE)
TSREE_before_JP1_REE = unlist(TSREE_before_JP1_REE)
OSSREE_between_JP1BS1_REE = unlist(OSSREE_between_JP1BS1_REE)
TSREE_between_JP1BS1_REE = unlist(TSREE_between_JP1BS1_REE)
OSSREE_between_BS1JP2_REE = unlist(OSSREE_between_BS1JP2_REE)
TSREE_between_BS1JP2_REE = unlist(TSREE_between_BS1JP2_REE)
OSSREE_between_JP2BS2_REE = unlist(OSSREE_between_JP2BS2_REE)
TSREE_between_JP2BS2_REE = unlist(TSREE_between_JP2BS2_REE)
OSSREE_between_BS2JP3_REE = unlist(OSSREE_between_BS2JP3_REE)
TSREE_between_BS2JP3_REE = unlist(TSREE_between_BS2JP3_REE)
OSSREE_after_JP3_REE = unlist(OSSREE_after_JP3_REE)
TSREE_after_JP3_REE = unlist(TSREE_after_JP3_REE)
llabels_JP1BS1JP2BS2JP3 = unlist(llabels_JP1BS1JP2BS2JP3)
## the ones experienced 3 BS and 2 JP
OSSREE_before_BS1_REE = unlist(OSSREE_before_BS1_REE)
TSREE_before_BS1_REE = unlist(TSREE_before_BS1_REE)
OSSREE_between_BS1JP1_REE = unlist(OSSREE_between_BS1JP1_REE)
TSREE_between_BS1JP1_REE = unlist(TSREE_between_BS1JP1_REE)
OSSREE_between_JP1BS2_REE = unlist(OSSREE_between_JP1BS2_REE)
TSREE_between_JP1BS2_REE = unlist(TSREE_between_JP1BS2_REE)
OSSREE_between_BS2JP2_REE = unlist(OSSREE_between_BS2JP2_REE)
TSREE_between_BS2JP2_REE = unlist(TSREE_between_BS2JP2_REE)
OSSREE_between_JP2BS3_REE = unlist(OSSREE_between_JP2BS3_REE)
TSREE_between_JP2BS3_REE = unlist(TSREE_between_JP2BS3_REE)
OSSREE_after_BS3_REE = unlist(OSSREE_after_BS3_REE)
TSREE_after_BS3_REE = unlist(TSREE_after_BS3_REE)
llabels_BS1JP1BS2JP3BS3 = unlist(llabels_BS1JP1BS2JP3BS3)
## the ones experienced 3JP 1BS 
# JP1 BS JP2 JP3 
OSSREE_before_JP1_REEe = unlist(OSSREE_before_JP1_REEe)
TSREE_before_JP1_REEe = unlist(TSREE_before_JP1_REEe)
OSSREE_between_JP1BS_REEe = unlist(OSSREE_between_JP1BS_REEe)
TSREE_between_JP1BS_REEe = unlist(TSREE_between_JP1BS_REEe)
OSSREE_between_BSJP2_REEe = unlist(OSSREE_between_BSJP2_REEe)
TSREE_between_BSJP2_REEe = unlist(TSREE_between_BSJP2_REEe)
OSSREE_between_JP2JP3_REEe = unlist(OSSREE_between_JP2JP3_REEe)
TSREE_between_JP2JP3_REEe = unlist(TSREE_between_JP2JP3_REEe)
OSSREE_after_JP3_REEe = unlist(OSSREE_after_JP3_REEe)
TSREE_after_JP3_REEe = unlist(TSREE_after_JP3_REEe)
llabels_JP1BSJP2JP3 = unlist(llabels_JP1BSJP2JP3)
# JP1 JP2 BS JP3
OSSREE_before_JP1_REEE = unlist(OSSREE_before_JP1_REEE)
TSREE_before_JP1_REEE = unlist(TSREE_before_JP1_REEE)
OSSREE_between_JP1JP2_REEE = unlist(OSSREE_between_JP1JP2_REEE)
TSREE_between_JP1JP2_REEE = unlist(TSREE_between_JP1JP2_REEE)
OSSREE_between_JP2BS_REEE = unlist(OSSREE_between_JP2BS_REEE)
TSREE_between_JP2BS_REEE = unlist(TSREE_between_JP2BS_REEE)
OSSREE_between_BSJP3_REEE = unlist(OSSREE_between_BSJP3_REEE)
TSREE_between_BSJP3_REEE = unlist(TSREE_between_BSJP3_REEE)
OSSREE_after_JP3_REEE = unlist(OSSREE_after_JP3_REEE)
TSREE_after_JP3_REEE = unlist(TSREE_after_JP3_REEE)
llabels_JP1JP2BSJP3 = unlist(llabels_JP1JP2BSJP3)
### the ones experienced 3BS 1JP
# BS1 JP BS2 BS3
OSSREE_before_BS1_REEe = unlist(OSSREE_before_BS1_REEe)
TSREE_before_BS1_REEe = unlist(TSREE_before_BS1_REEe)
OSSREE_between_BS1JP_REEe = unlist(OSSREE_between_BS1JP_REEe)
TSREE_between_BS1JP_REEe = unlist(TSREE_between_BS1JP_REEe)
OSSREE_between_JPBS2_REEe = unlist(OSSREE_between_JPBS2_REEe )
TSREE_between_JPBS2_REEe = unlist(TSREE_between_JPBS2_REEe)
OSSREE_between_BS2BS3_REEe = unlist(OSSREE_between_BS2BS3_REEe)
TSREE_between_BS2BS3_REEe = unlist(TSREE_between_BS2BS3_REEe)
OSSREE_after_BS3_REEe = unlist(OSSREE_after_BS3_REEe)
TSREE_after_BS3_REEe = unlist(TSREE_after_BS3_REEe)
llabels_BS1JPBS2BS3 = unlist(llabels_BS1JPBS2BS3)
# BS1 BS2 JP BS3
OSSREE_before_BS1_REEE = unlist(OSSREE_before_BS1_REEE)
TSREE_before_BS1_REEE = unlist(TSREE_before_BS1_REEE)
OSSREE_between_BS1BS2_REEE = unlist(OSSREE_between_BS1BS2_REEE)
TSREE_between_BS1BS2_REEE = unlist(TSREE_between_BS1BS2_REEE)
OSSREE_between_BS2JP_REEE = unlist(OSSREE_between_BS2JP_REEE)
TSREE_between_BS2JP_REEE = unlist(TSREE_between_BS2JP_REEE)
OSSREE_between_JPBS3_REEE = unlist(OSSREE_between_JPBS3_REEE)
TSREE_between_JPBS3_REEE = unlist(TSREE_between_JPBS3_REEE)
OSSREE_after_BS3_REEE = unlist(OSSREE_after_BS3_REEE)
TSREE_after_BS3_REEE = unlist(TSREE_after_BS3_REEE)
llabels_BS1BS2JPBS3 = unlist(llabels_BS1BS2JPBS3)
## the ones that experienced none
OSSREE_noneREE = unlist(OSSREE_noneREE)
TSREE_noneREE = unlist(TSREE_noneREE)
llabels_none = unlist(llabels_none)

######### PLOTS suivant les REE vu 
# bleu que  des BS ; jaune que des JP ; rose les 2 ; aucun vert
### que BSs
# que 1 BS
plot(OSSREE_before_BS,TSREE_before_BS,col="darkcyan",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2), xlab = "OSSREE", ylab = "TSREE")
title(main = "1 BS")
for (i in 1:length(OSSREE_before_BS)){text(OSSREE_before_BS[i],TSREE_before_BS[i]-.06,llabels_BS[i])}
points(OSSREE_after_BS,TSREE_after_BS,col="darkblue",pch = "+",cex=2)
for (i in 1:length(OSSREE_after_BS)){text(OSSREE_after_BS[i],TSREE_after_BS[i]-.06,llabels_BS[i])}
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)
# que 2 BS
plot(OSSREE_before_BS1,TSREE_before_BS1,col="darkcyan",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2),xlab = "OSSREE", ylab = "TSREE")
title(main = "2 BS")
for (i in 1:length(OSSREE_before_BS1)){text(OSSREE_before_BS1[i],TSREE_before_BS1[i]-.06,llabels_2BS[i])}
points(OSSREE_between_BSs,TSREE_between_BSs,col="blue",pch = "+",cex=2)
for (i in 1:length(OSSREE_between_BSs)){text(OSSREE_between_BSs[i],TSREE_between_BSs[i]-.06,llabels_2BS[i])}
points(OSSREE_after_BS2,TSREE_after_BS2,col="darkblue",pch = "+",cex=2)
for (i in 1:length(OSSREE_after_BS2)){text(OSSREE_after_BS2[i],TSREE_after_BS2[i]-.06,llabels_2BS[i])}
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)
# que 3 BS 
plot(OSSREE_before_BS1s,TSREE_before_BS1s,col="cyan",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2),xlab = "OSSREE", ylab = "TSREE")
title(main = "3 BS")
for (i in 1:length(OSSREE_before_BS1s)){text(OSSREE_before_BS1s[i],TSREE_before_BS1s[i]-.06,llabels_3BS[i])}
points(OSSREE_between_BS1_BS2,TSREE_between_BS1_BS2,col="darkcyan",pch = "+",cex=2)
for (i in 1:length(OSSREE_between_BS1_BS2)){text(OSSREE_between_BS1_BS2[i],TSREE_between_BS1_BS2[i]-.06,llabels_3BS[i])}
points(OSSREE_between_BS2_BS3,TSREE_between_BS2_BS3,col="blue",pch = "+",cex=2)
for (i in 1:length(OSSREE_between_BS2_BS3)){text(OSSREE_between_BS2_BS3[i],TSREE_between_BS2_BS3[i]-.06,llabels_3BS[i])}
points(OSSREE_after_BS3,TSREE_after_BS3,col="darkblue",pch = "+",cex=2)
for (i in 1:length(OSSREE_after_BS3)){text(OSSREE_after_BS3[i],TSREE_after_BS3[i]-.06,llabels_3BS[i])}
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)

### que JPs
# que 1 JP
plot(OSSREE_before_JP,TSREE_before_JP,col="darkgoldenrod2",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2), xlab = "OSSREE", ylab = "TSREE")
title(main = "1 JP")
for (i in 1:length(OSSREE_before_JP)){text(OSSREE_before_JP[i],TSREE_before_JP[i]-.06,llabels_JP[i])}
points(OSSREE_after_JP,TSREE_after_JP,col="darkgoldenrod4",pch = "+",cex=2)
for (i in 1:length(OSSREE_after_JP)){text(OSSREE_after_JP[i],TSREE_after_JP[i]-.06,llabels_JP[i])}
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)
# que 2 JP 
plot(OSSREE_before_JP1,TSREE_before_JP1,col="darkgoldenrod2",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2), xlab = "OSSREE", ylab = "TSREE")
title(main = "2 JP")
for (i in 1:length(OSSREE_before_JP1)){text(OSSREE_before_JP1[i],TSREE_before_JP1[i]-.06,llabels_2JP[i])}
points(OSSREE_between_JPs,TSREE_between_JPs,col="darkgoldenrod3",pch = "+",cex=2)
for (i in 1:length(OSSREE_between_JPs)){text(OSSREE_between_JPs[i],TSREE_between_JPs[i]-.06,llabels_2JP[i])}
points(OSSREE_after_JP2,TSREE_after_JP2,col="darkgoldenrod4",pch = "+",cex=2)
for (i in 1:length(OSSREE_after_JP2)){text(OSSREE_after_JP2[i],TSREE_after_JP2[i]-.06,llabels_2JP[i])}
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)
# que 3 JP
plot(OSSREE_before_JP1s,TSREE_before_JP1s,col="gold",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2), xlab = "OSSREE", ylab = "TSREE")
title(main = "3 JP")
for (i in 1:length(OSSREE_before_JP1s)){text(OSSREE_before_JP1s[i],TSREE_before_JP1s[i]-.06,llabels_3JP[i])}
points(OSSREE_between_JP1_JP2,TSREE_between_JP1_JP2,col="gold3",pch = "+",cex=2)
for (i in 1:length(OSSREE_between_JP1_JP2)){text(OSSREE_between_JP1_JP2[i],TSREE_between_JP1_JP2[i]-.06,llabels_3JP[i])}
points(OSSREE_between_JP2_JP3,TSREE_between_JP2_JP3,col="goldenrod3",pch = "+",cex=2)
for (i in 1:length(OSSREE_between_JP2_JP3)){text(OSSREE_between_JP2_JP3[i],TSREE_between_JP2_JP3[i]-.06,llabels_3JP[i])}
points(OSSREE_after_JP3,TSREE_after_JP3,col="goldenrod4",pch = "+",cex=2)
for (i in 1:length(OSSREE_after_JP3)){text(OSSREE_after_JP3[i],TSREE_after_JP3[i]-.06,llabels_3JP[i])}
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)

##### both JPs and BSs
### 1 de chaque 
# BS JP 
plot(OSSREE_before_BS_BSJP,TSREE_before_BS_BSJP,col="darkcyan",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2), xlab = "OSSREE", ylab = "TSREE")
title(main = "BS puis JP")
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)
for (i in 1:length(OSSREE_before_BS_BSJP)){text(OSSREE_before_BS_BSJP[i],TSREE_before_BS_BSJP[i]-.06,llabels_BSJP[i])}
points(OSSREE_between_BSJP,TSREE_between_BSJP,col="palevioletred3",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_BSJP)){text(OSSREE_between_BSJP[i],TSREE_between_BSJP[i]-.06,llabels_BSJP[i])}
points(OSSREE_after_JP_BSJP,TSREE_after_JP_BSJP,col="darkgoldenrod4",pch = "+",cex=2)
for (i in 1:length(OSSREE_after_JP_BSJP)){text(OSSREE_after_JP_BSJP[i],TSREE_after_JP_BSJP[i]-.06,llabels_BSJP[i])}

# JP BS 
plot(OSSREE_before_JP_JPBS,TSREE_before_JP_JPBS,col="darkgoldenrod2",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2), xlab = "OSSREE", ylab = "TSREE")
title(main = "JP puis BS")
for (i in 1:length(OSSREE_before_JP_JPBS)){text(OSSREE_before_JP_JPBS[i],TSREE_before_JP_JPBS[i]-.06,llabels_JPBS[i])}
points(OSSREE_between_JPBS,TSREE_between_JPBS,col="palevioletred3",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_JPBS)){text(OSSREE_between_JPBS[i],TSREE_between_JPBS[i]-.06,llabels_JPBS[i])}
points(OSSREE_after_BS_BSJP,TSREE_after_BS_BSJP,col="darkblue",pch = "+",cex=2)
for (i in 1:length(OSSREE_after_BS_BSJP)){text(OSSREE_after_BS_BSJP[i],TSREE_after_BS_BSJP[i]-.06,llabels_BSJP[i])}
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)

####### 2 JP 1 BS
# BS1 JP1 JP2 
plot(OSSREE_before_BS1_Ree,TSREE_before_BS1_Ree,col="darkcyan",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2), xlab = "OSSREE", ylab = "TSREE")
title(main = "BS1, JP1, JP2")
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)
for (i in 1:length(OSSREE_before_BS1_Ree)){text(OSSREE_before_BS1_Ree[i],TSREE_before_BS1_Ree[i]-.06,llabels_BS1JP1JP2[i])}
points(OSSREE_between_BS1JP1_Ree,TSREE_between_BS1JP1_Ree,col="palevioletred3",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_BS1JP1_Ree)){text(OSSREE_between_BS1JP1_Ree[i],TSREE_between_BS1JP1_Ree[i]-.06,llabels_BS1JP1JP2[i])}
points(OSSREE_between_JP1JP2_Ree,TSREE_between_JP1JP2_Ree,col="darkgoldenrod2",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_JP1JP2_Ree)){text(OSSREE_between_JP1JP2_Ree[i],TSREE_between_JP1JP2_Ree[i]-.06,llabels_BS1JP1JP2[i])}
points(OSSREE_after_JP2_Ree,TSREE_after_JP2_Ree,col="darkgoldenrod4",pch = "+",cex=2)
for (i in 1:length(OSSREE_after_JP2_Ree)){text(OSSREE_after_JP2_Ree[i],TSREE_after_JP2_Ree[i]-.06,llabels_BS1JP1JP2[i])}

# JP1 BS2 JP2 
plot(OSSREE_before_JP1_reE,TSREE_before_JP1_reE,col="darkgoldenrod2",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2), xlab = "OSSREE", ylab = "TSREE")
title(main = "JP1, BS2, JP2")
for (i in 1:length(OSSREE_before_JP1_reE)){text(OSSREE_before_JP1_reE[i],TSREE_before_JP1_reE[i]-.06,llabels_JP1BS2JP2[i])}
points(OSSREE_between_JP1BS2_reE,TSREE_between_JP1BS2_reE,col="darkgoldenrod3",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_JP1BS2_reE)){text(OSSREE_between_JP1BS2_reE[i],TSREE_between_JP1BS2_reE[i]-.06,llabels_JP1BS2JP2[i])}
points(OSSREE_between_BS2JP2_reE,TSREE_between_BS2JP2_reE,col="palevioletred3",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_BS2JP2_reE)){text(OSSREE_between_BS2JP2_reE[i],TSREE_between_BS2JP2_reE[i]-.06,llabels_JP1BS2JP2[i])}
points(OSSREE_after_JP2_reE,TSREE_after_JP2_reE,col="darkgoldenrod4",pch = "+",cex=2)
for (i in 1:length(OSSREE_after_JP2_reE)){text(OSSREE_after_JP2_reE[i],TSREE_after_JP2_reE[i]-.06,llabels_JP1BS2JP2[i])}
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)
# JP1 JP2 BS 
plot(OSSREE_before_JP1_reEE,TSREE_before_JP1_reEE,col="gold",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2), xlab = "OSSREE", ylab = "TSREE")
title(main = "JP1, JP2, BS")
for (i in 1:length(OSSREE_before_JP1_reEE)){text(OSSREE_before_JP1_reEE[i],TSREE_before_JP1_reEE[i]-.06,llabels_JP1JP2BS[i])}
points(OSSREE_between_JP1JP2_reEE,TSREE_between_JP1JP2_reEE,col="darkgoldenrod3",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_JP1JP2_reEE)){text(OSSREE_between_JP1JP2_reEE[i],TSREE_between_JP1JP2_reEE[i]-.06,llabels_JP1JP2BS[i])}
points(OSSREE_between_JP2BS_reEE,TSREE_between_JP2BS_reEE,col="palevioletred3",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_JP2BS_reEE)){text(OSSREE_between_JP2BS_reEE[i],TSREE_between_JP2BS_reEE[i]-.06,llabels_JP1JP2BS[i])}
points(OSSREE_after_BS_reEE,TSREE_after_BS_reEE,col="blue",pch = "+",cex=2)
for (i in 1:length(OSSREE_after_BS_reEE)){text(OSSREE_after_BS_reEE[i],TSREE_after_BS_reEE[i]-.06,llabels_JP1JP2BS[i])}
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)

###### 2 BS 1 JP
# BS1 JP1 BS2 
plot(OSSREE_before_BS1_REe,TSREE_before_BS1_REe,col="darkcyan",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2), xlab = "OSSREE", ylab = "TSREE")
title(main = "BS1, JP1, BS2")
for (i in 1:length(OSSREE_before_BS1_REe)){text(OSSREE_before_BS1_REe[i],TSREE_before_BS1_REe[i]-.06,llabels_BS1JP1BS2[i])}
points(OSSREE_between_BS1JP1_REe,TSREE_between_BS1JP1_REe,col="palevioletred3",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_BS1JP1_REe)){text(OSSREE_between_BS1JP1_REe[i],TSREE_between_BS1JP1_REe[i]-.06,llabels_BS1JP1BS2[i])}
points(OSSREE_between_JP1BS2_REe,TSREE_between_JP1BS2_REe,col="darkgoldenrod2",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_JP1BS2_REe)){text(OSSREE_between_JP1BS2_REe[i],TSREE_between_JP1BS2_REe[i]-.06,llabels_BS1JP1BS2[i])}
points(OSSREE_after_BS2_REe,TSREE_after_BS2_REe,col="darkblue",pch = "+",cex=2)
for (i in 1:length(OSSREE_after_BS2_REe)){text(OSSREE_after_BS2_REe[i],TSREE_after_BS2_REe[i]-.06,llabels_BS1JP1BS2[i])}
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)
# BS1 BS2 JP2 
plot(OSSREE_before_BS1_ree,TSREE_before_BS1_ree,col="darkcyan",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2), xlab = "OSSREE", ylab = "TSREE")
title(main = "BS1, BS2, JP2")
for (i in 1:length(OSSREE_before_BS1_ree)){text(OSSREE_before_BS1_ree[i],TSREE_before_BS1_ree[i]-.06,llabels_BS1BS2JP2[i])}
points(OSSREE_between_BS1BS2_ree,TSREE_between_BS1BS2_ree,col="darkblue",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_BS1BS2_ree)){text(OSSREE_between_BS1BS2_ree[i],TSREE_between_BS1BS2_ree[i]-.06,llabels_BS1BS2JP2[i])}
points(OSSREE_between_BS2JP2_ree,TSREE_between_BS2JP2_ree,col="palevioletred3",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_BS2JP2_ree)){text(OSSREE_between_BS2JP2_ree[i],TSREE_between_BS2JP2_ree[i]-.06,llabels_BS1BS2JP2[i])}
points(OSSREE_after_JP2_ree,TSREE_after_JP2_ree,col="darkgoldenrod4",pch = "+",cex=2)
for (i in 1:length(OSSREE_after_JP2_ree)){text(OSSREE_after_JP2_ree[i],TSREE_after_JP2_ree[i]-.06,llabels_BS1BS2JP2[i])}
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)
# JP BS1 BS2 
plot(OSSREE_before_JP_reee,TSREE_before_JP_reee,col="darkcyan",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2), xlab = "OSSREE", ylab = "TSREE")
title(main = "JP, BS1, BS2")
for (i in 1:length(OSSREE_before_JP_reee)){text(OSSREE_before_JP_reee[i],TSREE_before_JP_reee[i]-.06,llabels_JPBS1BS2[i])}
points(OSSREE_between_JPBS1_reee,TSREE_between_JPBS1_reee,col="darkblue",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_JPBS1_reee)){text(OSSREE_between_JPBS1_reee[i],TSREE_between_JPBS1_reee[i]-.06,llabels_JPBS1BS2[i])}
points(OSSREE_between_BS1BS2_reee,TSREE_between_BS1BS2_reee,col="palevioletred3",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_BS1BS2_reee)){text(OSSREE_between_BS1BS2_reee[i],TSREE_between_BS1BS2_reee[i]-.06,llabels_JPBS1BS2[i])}
points(OSSREE_after_BS2_reee,TSREE_after_BS2_reee,col="darkgoldenrod4",pch = "+",cex=2)
for (i in 1:length(OSSREE_after_BS2_reee)){text(OSSREE_after_BS2_reee[i],TSREE_after_BS2_reee[i]-.06,llabels_JPBS1BS2[i])}
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)

###### 2 de chaque : 2BS 2JP
#BS1 JP1 BS2 JP2
plot(OSSREE_before_BS1_rEE,TSREE_before_BS1_rEE,col="darkcyan",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2), xlab = "OSSREE", ylab = "TSREE")
title(main = "BS1, JP1, BS2, JP2")
for (i in 1:length(OSSREE_before_BS1_rEE)){text(OSSREE_before_BS1_rEE[i],TSREE_before_BS1_rEE[i]-.06,llabels_BS1JP1BS2JP2[i])}
points(OSSREE_between_BS1JP1_rEE,TSREE_between_BS1JP1_rEE,col="palevioletred4",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_BS1JP1_rEE)){text(OSSREE_between_BS1JP1_rEE[i],TSREE_between_BS1JP1_rEE[i]-.06,llabels_BS1JP1BS2JP2[i])}
points(OSSREE_between_JP1BS2_rEE,TSREE_between_JP1BS2_rEE,col="palevioletred3",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_JP1BS2_rEE)){text(OSSREE_between_JP1BS2_rEE[i],TSREE_between_JP1BS2_rEE[i]-.06,llabels_BS1JP1BS2JP2[i])}
points(OSSREE_between_BS2JP2_rEE,TSREE_between_BS2JP2_rEE,col="palevioletred2",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_BS2JP2_rEE)){text(OSSREE_between_BS2JP2_rEE[i],TSREE_between_BS2JP2_rEE[i]-.06,llabels_BS1JP1BS2JP2[i])}
points(OSSREE_after_JP2_rEE,TSREE_after_JP2_rEE,col="darkgoldenrod4",pch = "+",cex=2)
for (i in 1:length(OSSREE_after_JP2_rEE)){text(OSSREE_after_JP2_rEE[i],TSREE_after_JP2_rEE[i]-.06,llabels_BS1JP1BS2JP2[i])}
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)
# JP1 BS1 JP2 BS2 
plot(OSSREE_before_JP1_rEE,TSREE_before_JP1_rEE,col="gold",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2), xlab = "OSSREE", ylab = "TSREE")
title(main = "JP1, BS1, JP2, BS2")
for (i in 1:length(OSSREE_before_JP1_rEE)){text(OSSREE_before_JP1_rEE[i],TSREE_before_JP1_rEE[i]-.06,llabels_JP1BS2JP2BS2[i])}
points(OSSREE_between_JP1BS1_rEE,TSREE_between_JP1BS1_rEE,col="pink",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_JP1BS1_rEE)){text(OSSREE_between_JP1BS1_rEE[i],TSREE_between_JP1BS1_rEE[i]-.06,llabels_JP1BS2JP2BS2[i])}
points(OSSREE_between_BS1JP2_rEE,TSREE_between_BS1JP2_rEE,col="gold4",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_BS1JP2_rEE)){text(OSSREE_between_BS1JP2_rEE[i],TSREE_between_BS1JP2_rEE[i]-.06,llabels_JP1BS2JP2BS2[i])}
points(OSSREE_between_JP2BS2_rEE,TSREE_between_JP2BS2_rEE,col="palevioletred2",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_JP2BS2_rEE)){text(OSSREE_between_JP2BS2_rEE[i],TSREE_between_JP2BS2_rEE[i]-.06,llabels_JP1BS2JP2BS2[i])}
points(OSSREE_after_BS2_rEE,TSREE_after_BS2_rEE,col="darkcyan",pch = "+",cex=2)
for (i in 1:length(OSSREE_after_BS2_rEE)){text(OSSREE_after_BS2_rEE[i],TSREE_after_BS2_rEE[i]-.06,llabels_JP1BS2JP2BS2[i])}
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)

# BS JP JP BS

# JP BS BS JP

#### the ones experienced 3 JP and 2 BS
# JP1 BS1 JP2 BS2 JP3
plot(OSSREE_before_JP1_REE,TSREE_before_JP1_REE,col="gold",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2), xlab = "OSSREE", ylab = "TSREE")
title(main = "JP1, BS1, JP2, BS2, JP3")
for (i in 1:length(OSSREE_before_JP1_REE)){text(OSSREE_before_JP1_REE[i],TSREE_before_JP1_REE[i]-.06,llabels_JP1BS1JP2BS2JP3[i])}
points(OSSREE_between_JP1BS1_REE,TSREE_between_JP1BS1_REE,col="palevioletred4",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_JP1BS1_REE)){text(OSSREE_between_JP1BS1_REE[i],TSREE_between_JP1BS1_REE[i]-.06,llabels_JP1BS1JP2BS2JP3[i])}
points(OSSREE_between_BS1JP2_REE,TSREE_between_BS1JP2_REE,col="palevioletred3",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_BS1JP2_REE)){text(OSSREE_between_BS1JP2_REE[i],TSREE_between_BS1JP2_REE[i]-.06,llabels_JP1BS1JP2BS2JP3[i])}
points(OSSREE_between_JP2BS2_REE,TSREE_between_JP2BS2_REE,col="gold3",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_JP2BS2_REE)){text(OSSREE_between_JP2BS2_REE[i],TSREE_between_JP2BS2_REE[i]-.06,llabels_JP1BS1JP2BS2JP3[i])}
points(OSSREE_between_BS2JP3_REE,TSREE_between_BS2JP3_REE,col="palevioletred2",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_BS2JP3_REE)){text(OSSREE_between_BS2JP3_REE[i],TSREE_between_BS2JP3_REE[i]-.06,llabels_JP1BS1JP2BS2JP3[i])}
points(OSSREE_after_JP3_REE,TSREE_after_JP3_REE,col="darkgoldenrod4",pch = "+",cex=2)
for (i in 1:length(OSSREE_after_JP3_REE)){text(OSSREE_after_JP3_REE[i],TSREE_after_JP3_REE[i]-.06,llabels_JP1BS1JP2BS2JP3[i])}
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)
#### the ones experienced 3 BS and 2 JP
# BS1 JP1 BS2 JP2 BS3
plot(OSSREE_before_BS1_REE,TSREE_before_BS1_REE,col="darkcyan",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2), xlab = "OSSREE", ylab = "TSREE")
title(main = "BS1, JP1, BS2, JP2, BS3")
for (i in 1:length(OSSREE_before_BS1_REE)){text(OSSREE_before_BS1_REE[i],TSREE_before_BS1_REE[i]-.06,llabels_BS1JP1BS2JP3BS3[i])}
points(OSSREE_between_BS1JP1_REE,TSREE_between_BS1JP1_REE,col="gold",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_BS1JP1_REE)){text(OSSREE_between_BS1JP1_REE[i],TSREE_between_BS1JP1_REE[i]-.06,llabels_BS1JP1BS2JP3BS3[i])}
points(OSSREE_between_JP1BS2_REE,TSREE_between_JP1BS2_REE,col="palevioletred3",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_JP1BS2_REE)){text(OSSREE_between_JP1BS2_REE[i],TSREE_between_JP1BS2_REE[i]-.06,llabels_BS1JP1BS2JP3BS3[i])}
points(OSSREE_between_BS2JP2_REE,TSREE_between_BS2JP2_REE,col="cyan",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_BS2JP2_REE)){text(OSSREE_between_BS2JP2_REE[i],TSREE_between_BS2JP2_REE[i]-.06,llabels_BS1JP1BS2JP3BS3[i])}
points(OSSREE_between_JP2BS3_REE,TSREE_between_JP2BS3_REE,col="palevioletred2",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_JP2BS3_REE)){text(OSSREE_between_JP2BS3_REE[i],TSREE_between_JP2BS3_REE[i]-.06,llabels_BS1JP1BS2JP3BS3[i])}
points(OSSREE_after_BS3_REE,TSREE_after_BS3_REE,col="darkgoldenrod4",pch = "+",cex=2)
for (i in 1:length(OSSREE_after_BS3_REE)){text(OSSREE_after_BS3_REE[i],TSREE_after_BS3_REE[i]-.06,llabels_BS1JP1BS2JP3BS3[i])}
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)

#### the ones experienced 3JP 1BS 
# JP1 BS JP2 JP3 
plot(OSSREE_before_JP1_REEe,TSREE_before_JP1_REEe,col="gold",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2), xlab = "OSSREE", ylab = "TSREE")
title(main = "JP1, BS, JP2, JP3")
for (i in 1:length(OSSREE_before_JP1_REEe)){text(OSSREE_before_JP1_REEe[i],TSREE_before_JP1_REEe[i]-.06,llabels_JP1BSJP2JP3[i])}
points(OSSREE_between_JP1BS_REEe,TSREE_between_JP1BS_REEe,col="palevioletred4",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_JP1BS_REEe)){text(OSSREE_between_JP1BS_REEe[i],TSREE_between_JP1BS_REEe[i]-.06,llabels_JP1BSJP2JP3[i])}
points(OSSREE_between_BSJP2_REEe,TSREE_between_BSJP2_REEe,col="palevioletred3",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_BSJP2_REEe)){text(OSSREE_between_BSJP2_REEe[i],TSREE_between_BSJP2_REEe[i]-.06,llabels_JP1BSJP2JP3[i])}
points(OSSREE_between_JP2JP3_REEe,TSREE_between_JP2JP3_REEe,col="gold3",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_JP2JP3_REEe)){text(OSSREE_between_JP2JP3_REEe[i],TSREE_between_JP2JP3_REEe[i]-.06,llabels_JP1BSJP2JP3[i])}
points(OSSREE_after_JP3_REEe,TSREE_after_JP3_REEe,col="darkgoldenrod4",pch = "+",cex=2)
for (i in 1:length(OSSREE_after_JP3_REEe)){text(OSSREE_after_JP3_REEe[i],TSREE_after_JP3_REEe[i]-.06,llabels_JP1BSJP2JP3[i])}
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)
# JP1 JP2 BS JP3
plot(OSSREE_before_JP1_REEE,TSREE_before_JP1_REEE,col="gold",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2), xlab = "OSSREE", ylab = "TSREE")
title(main = "JP1, JP2, BS, JP3")
for (i in 1:length(OSSREE_before_JP1_REEE)){text(OSSREE_before_JP1_REEE[i],TSREE_before_JP1_REEE[i]-.06,llabels_JP1JP2BSJP3[i])}
points(OSSREE_between_JP1JP2_REEE,TSREE_between_JP1JP2_REEE,col="palevioletred4",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_JP1JP2_REEE)){text(OSSREE_between_JP1JP2_REEE[i],TSREE_between_JP1JP2_REEE[i]-.06,llabels_JP1JP2BSJP3[i])}
points(OSSREE_between_JP2BS_REEE,TSREE_between_JP2BS_REEE,col="palevioletred3",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_JP2BS_REEE)){text(OSSREE_between_JP2BS_REEE[i],TSREE_between_JP2BS_REEE[i]-.06,llabels_JP1JP2BSJP3[i])}
points(OSSREE_between_BSJP3_REEE,TSREE_between_BSJP3_REEE,col="gold3",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_BSJP3_REEE)){text(OSSREE_between_BSJP3_REEE[i],TSREE_between_BSJP3_REEE[i]-.06,llabels_JP1JP2BSJP3[i])}
points(OSSREE_after_JP3_REEE,TSREE_after_JP3_REEE,col="darkgoldenrod4",pch = "+",cex=2)
for (i in 1:length(OSSREE_after_JP3_REEE)){text(OSSREE_after_JP3_REEE[i],TSREE_after_JP3_REEE[i]-.06,llabels_JP1JP2BSJP3[i])}
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)

### the ones experienced 3BS 1JP
# BS1 JP BS2 BS3
plot(OSSREE_before_BS1_REEe,TSREE_before_BS1_REEe,col="cyan",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2), xlab = "OSSREE", ylab = "TSREE")
title(main = "BS1, JP, BS2, BS3")
for (i in 1:length(OSSREE_before_BS1_REEe)){text(OSSREE_before_BS1_REEe[i],TSREE_before_BS1_REEe[i]-.06,llabels_BS1JPBS2BS3[i])}
points(OSSREE_between_BS1JP_REEe,TSREE_between_BS1JP_REEe,col="darkcyan",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_BS1JP_REEe)){text(OSSREE_between_BS1JP_REEe[i],TSREE_between_BS1JP_REEe[i]-.06,llabels_BS1JPBS2BS3[i])}
points(OSSREE_between_JPBS2_REEe,TSREE_between_JPBS2_REEe,col="gold3",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_JPBS2_REEe)){text(OSSREE_between_JPBS2_REEe[i],TSREE_between_JPBS2_REEe[i]-.06,llabels_BS1JPBS2BS3[i])}
points(OSSREE_between_BS2BS3_REEe,TSREE_between_BS2BS3_REEe,col="palevioletred3",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_BS2BS3_REEe)){text(OSSREE_between_BS2BS3_REEe[i],TSREE_between_BS2BS3_REEe[i]-.06,llabels_BS1JPBS2BS3[i])}
points(OSSREE_after_BS3_REEe,TSREE_after_BS3_REEe,col="darkgoldenrod4",pch = "+",cex=2)
for (i in 1:length(OSSREE_after_BS3_REEe)){text(OSSREE_after_BS3_REEe[i],TSREE_after_BS3_REEe[i]-.06,llabels_BS1JPBS2BS3[i])}
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)
# BS1 BS2 JP BS3
plot(OSSREE_before_BS1_REEE,TSREE_before_BS1_REEE,col="cyan",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2), xlab = "OSSREE", ylab = "TSREE")
title(main = "BS1, BS2, JP, BS3")
for (i in 1:length(OSSREE_before_BS1_REEE)){text(OSSREE_before_BS1_REEE[i],TSREE_before_BS1_REEE[i]-.06,llabels_BS1BS2JPBS3[i])}
points(OSSREE_between_BS1BS2_REEE,TSREE_between_BS1BS2_REEE,col="darcyan",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_BS1BS2_REEE)){text(OSSREE_between_BS1BS2_REEE[i],TSREE_between_BS1BS2_REEE[i]-.06,llabels_BS1BS2JPBS3[i])}
points(OSSREE_between_BS2JP_REEE,TSREE_between_BS2JP_REEE,col="blue",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_BS2JP_REEE)){text(OSSREE_between_BS2JP_REEE[i],TSREE_between_BS2JP_REEE[i]-.06,llabels_BS1BS2JPBS3[i])}
points(OSSREE_between_JPBS3_REEE,TSREE_between_JPBS3_REEE,col="gold3",pch = "+",cex=2,xlim=c(-1,1),ylim=c(0,2))
for (i in 1:length(OSSREE_between_JPBS3_REEE)){text(OSSREE_between_JPBS3_REEE[i],TSREE_between_JPBS3_REEE[i]-.06,llabels_BS1BS2JPBS3[i])}
points(OSSREE_after_BS3_REEE,TSREE_after_BS3_REEE,col="darkgoldenrod4",pch = "+",cex=2)
for (i in 1:length(OSSREE_after_BS3_REEE)){text(OSSREE_after_BS3_REEE[i],TSREE_after_BS3_REEE[i]-.06,llabels_BS1BS2JPBS3[i])}
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)

#### none 
plot(OSSREE_noneREE,TSREE_noneREE, col = "yellowgreen", pch="+", cex=2,xlim=c(-1,1),ylim=c(0,2), xlab = "OSSREE", ylab = "TSREE")
title(main = "none REE was experienced")
for (i in 1:length(OSSREE_noneREE)){text(OSSREE_noneREE[i],TSREE_noneREE[i]-.06,llabels_none[i])}
lines(c(0,1,0,-1,0),c(0,1,2,1,0))
lines(c(0,0),c(0,2),lty=2)
lines(c(-1,1),c(1,1),lty=2)

############################### PCA des choix ###########################



############################# avant après REE ##########################
### Corrélation avant / après (20 clics) TSREE et OSSREE pour chaque REE d'un côté les JP et d'un autre côté les BS 
# extract_REE <- function(id, dataframe, ree_value, n_clics, limit)

# BS
OSREE_avant_BS = list()
OSREE_après_BS = list()
TSREE_avant_BS = list()
TSREE_après_BS = list()
llabels_BS = list()
# JP
OSREE_avant_JP = list()
OSREE_après_JP = list()
TSREE_avant_JP = list()
TSREE_après_JP = list()
llabels_JP = list()


apparition_BS <- rep(0,3)
apparition_JP <- rep(0,3)
data = data_fourchoices_2
n = 200
for (i in unique(data$id)) {
  apparition_BS = extract_REE(i, data, -3000, 10 , 1)
  apparition_JP = extract_REE(i, data, 3000, 10, 100)
  print(i)
  print(apparition_BS)
  print(apparition_JP)
  four_choice_i = data[data$id == i , ]$button_name
  
  if (apparition_BS[1] != 1){
    for (k in 1:length(apparition_BS)) {
      llabels_BS <- append(llabels_BS, i)
      before = four_choice_i[as.integer(apparition_BS[k]-20):apparition_BS[k]]
      after = four_choice_i[apparition_BS[k]:as.integer(apparition_BS[k]+19)]
      prop_before_BS = table(before)/length(before)
      prop_after_BS = table(after)/length(after)
      OSREE_avant_BS <- append(OSREE_avant_BS, prop_before_BS[4]-prop_before_BS[3])
      OSREE_après_BS <- append(OSREE_après_BS, prop_after_BS[4]-prop_after_BS[3])
      TSREE_avant_BS <- append(TSREE_avant_BS, 1+ prop_before_BS[1]- prop_before_BS[2])
      TSREE_après_BS <- append(TSREE_après_BS, 1+ prop_after_BS[1]- prop_after_BS[2])
    }}
  
  if (apparition_JP[1] != 100){
    for (l in 1:length(apparition_JP)) {
      llabels_JP <- append(llabels_JP, i)
      before = four_choice_i[as.integer(apparition_JP[l]-20):apparition_JP[l]]
      after = four_choice_i[apparition_JP[l]:as.integer(apparition_JP[l]+19)]
      prop_before_JP = table(before)/length(before)
      prop_after_JP = table(after)/length(after)
      OSREE_avant_JP <- append(OSREE_avant_JP, prop_before_JP[4]-prop_before_JP[3])
      OSREE_après_JP <- append(OSREE_après_JP, prop_after_JP[4]-prop_after_JP[3])
      TSREE_avant_JP <- append(TSREE_avant_JP, 1+ prop_before_JP[1]- prop_before_JP[2])
      TSREE_après_JP <- append(TSREE_après_JP, 1+ prop_after_JP[1]- prop_after_JP[2])
    }}
}

# unlist 
OSREE_avant_BS = unlist(OSREE_avant_BS)
OSREE_après_BS = unlist(OSREE_après_BS)
TSREE_avant_BS = unlist(TSREE_avant_BS)
TSREE_après_BS = unlist(TSREE_après_BS)
llabels_BS = unlist(llabels_BS)

# JP
OSREE_avant_JP = unlist(OSREE_avant_JP)
OSREE_après_JP = unlist(OSREE_après_JP)
TSREE_avant_JP = unlist(TSREE_avant_JP)
TSREE_après_JP = unlist(TSREE_après_JP)
llabels_JP = unlist(llabels_JP)

## plot carré des données 
# pour BS
par(mfrow= c(2,2))
plot(OSREE_avant_BS,OSREE_après_BS,col="firebrick",pch = "+",cex=2,xlim=c(-1,1),ylim=c(-1,1), xlab = "OSREE before BS", ylab = "OSREE after BS")
#for (i in 1:length(OSREE_avant_BS)){text(OSREE_avant_BS[i],OSREE_après_BS[i]-.06,llabels_BS[i])}
reg <- lm(OSREE_avant_BS ~ OSREE_après_BS)
abline(reg, col = "black")
abline(a = 0, b = 1, col = "gray")
plot(TSREE_avant_BS,TSREE_après_BS,col="firebrick",pch = "+",cex=2,xlim=c(0,2),ylim=c(0,2), xlab = "TSREE before BS", ylab = "TSREE after BS")
#for (i in 1:length(TSREE_avant_BS)){text(TSREE_avant_BS[i],TSREE_après_BS[i]-.06,llabels_BS[i])}
reg <- lm(TSREE_avant_BS ~ TSREE_après_BS)
abline(reg, col = "black")
abline(a = 0, b = 1, col = "gray")
# pour JP 
plot(OSREE_avant_JP,OSREE_après_JP,col="forestgreen",pch = "+",cex=2,xlim=c(-1,1),ylim=c(-1,1), xlab = "OSREE before JP", ylab = "OSREE after JP")
#for (i in 1:length(OSREE_avant_JP)){text(OSREE_avant_JP[i],OSREE_après_JP[i]-.06,llabels_JP[i])}
reg <- lm(OSREE_avant_JP ~ OSREE_après_JP)
abline(reg, col = "black")
abline(a = 0, b = 1, col = "gray")
plot(TSREE_avant_JP,TSREE_après_JP,col="forestgreen",pch = "+",cex=2,xlim=c(0,2),ylim=c(0,2), xlab = "TSREE before JP", ylab = "TSREE after JP")
#for (i in 1:length(TSREE_avant_JP)){text(TSREE_avant_JP[i],TSREE_après_JP[i]-.06,llabels_JP[i])}
reg <- lm(TSREE_avant_JP ~ TSREE_après_JP)
abline(reg, col = "black")
abline(a = 0, b = 1, col = "gray")
par(mfrow= c(1,1))

# tests stats
wilcox.test(OSREE_avant_BS, OSREE_après_BS, paired = TRUE)
wilcox.test(TSREE_avant_BS, TSREE_après_BS, paired = TRUE)
wilcox.test(OSREE_avant_JP, OSREE_après_JP, paired = TRUE)
wilcox.test(TSREE_avant_JP, TSREE_après_JP, paired = TRUE)

### jittered plots 
# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(viridisLite)
# create a dataset for BS 
OSREE_beforeafter_BS <- data.frame(
  name=c("OSREE before", "OSREE after"),
  value=c( OSREE_avant_BS, OSREE_après_BS) )
OSREE_beforeafter_BS <- OSREE_beforeafter_BS %>%
  mutate(name = factor(name, levels = c("OSREE before", "OSREE after")))
TSREE_beforeafter_BS <- data.frame(
  name=c("TSREE before", "TSREE after"),
  value=c( TSREE_avant_BS, TSREE_après_BS) )
TSREE_beforeafter_BS <- TSREE_beforeafter_BS %>%
  mutate(name = factor(name, levels = c("TSREE before", "TSREE after")))

# Plot
OSREE_beforeafter_BS %>%
  ggplot(aes(x = name, y = value, fill = name)) +
  geom_boxplot(fill = "firebrick") +  # Couleur des boîtes définie à "firebrick"
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, hjust = 0.5)  # Centrer le titre avec hjust = 0.5
  ) +
  ggtitle("Before vs After a BS") +
  xlab("")+
  ylab("")

TSREE_beforeafter_BS %>%
  ggplot(aes(x = name, y = value, fill = name)) +
  geom_boxplot(fill = "firebrick") +  # Couleur des boîtes définie à "firebrick"
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, hjust = 0.5)  # Centrer le titre avec hjust = 0.5
  ) +
  ggtitle("Before vs After a BS") +
  xlab("")+
  ylab("")

# create a dataset for JP 
OSREE_beforeafter_JP <- data.frame(
  name=c("OSREE before", "OSREE after"),
  value=c( OSREE_avant_JP, OSREE_après_JP) )
OSREE_beforeafter_JP <- OSREE_beforeafter_JP %>%
  mutate(name = factor(name, levels = c("OSREE before", "OSREE after")))
TSREE_beforeafter_JP <- data.frame(
  name=c("TSREE before", "TSREE after"),
  value=c( TSREE_avant_JP, TSREE_après_JP) )
TSREE_beforeafter_JP <- TSREE_beforeafter_JP %>%
  mutate(name = factor(name, levels = c("TSREE before", "TSREE after")))

# Plot
OSREE_beforeafter_JP %>%
  ggplot(aes(x = name, y = value, fill = name)) +
  geom_boxplot(fill = "forestgreen") +  # Couleur des boîtes définie à "firebrick"
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, hjust = 0.5)  # Centrer le titre avec hjust = 0.5
  ) +
  ggtitle("Before vs After a JP") +
  xlab("")+
  ylab("")

TSREE_beforeafter_JP %>%
  ggplot(aes(x = name, y = value, fill = name)) +
  geom_boxplot(fill = "forestgreen") +  # Couleur des boîtes définie à "firebrick"
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, hjust = 0.5)  # Centrer le titre avec hjust = 0.5
  ) +
  ggtitle("Before vs After a JP") +
  xlab("")+
  ylab("")

####### BS avoidance & JP seeking 
# BS => plus de antifragile et robust moins de fragile et vulnérable 
# JP => plus de antifragile et vulnérable ou robust ? 

# BS => A+R vs V+F
prop_A_R_avant_BS = list()
prop_A_R_après_BS = list()
prop_V_F_avant_BS = list()
prop_V_F_après_BS = list()
prop_A_avant_BS = list()
prop_R_avant_BS = list()
prop_V_avant_BS = list()
prop_F_avant_BS = list()
prop_A_après_BS = list()
prop_R_après_BS = list()
prop_V_après_BS = list()
prop_F_après_BS = list()
# JP => A+V ou A+R augmente vs V et F diminue ? 
prop_A_R_avant_JP = list()
prop_A_R_après_JP = list()
prop_A_V_avant_JP = list()
prop_A_V_après_JP = list()
prop_V_F_avant_JP = list()
prop_V_F_après_JP = list()
prop_A_avant_JP = list()
prop_R_avant_JP = list()
prop_V_avant_JP = list()
prop_F_avant_JP = list()
prop_A_après_JP = list()
prop_R_après_JP = list()
prop_V_après_JP = list()
prop_F_après_JP = list()

apparition_BS <- rep(0,3)
apparition_JP <- rep(0,3)
data = data_fourchoices_2
n = 200
for (i in unique(data$id)) {
  apparition_BS = extract_REE(i, data, -3000, 10 , 1)
  apparition_JP = extract_REE(i, data, 3000, 10, 100)
  print(i)
  print(apparition_BS)
  print(apparition_JP)
  four_choice_i = data[data$id == i , ]$button_name
  
  if (apparition_BS[1] != 1){
    for (k in 1:length(apparition_BS)) {
      llabels_BS <- append(llabels_BS, i)
      before = four_choice_i[as.integer(apparition_BS[k]-20):apparition_BS[k]]
      after = four_choice_i[apparition_BS[k]:as.integer(apparition_BS[k]+19)]
      prop_before_BS = table(before)/length(before)
      prop_after_BS = table(after)/length(after)
      prop_A_R_avant_BS <- append(prop_A_R_avant_BS, prop_before_BS[1]+prop_before_BS[3])
      prop_A_R_après_BS <- append(prop_A_R_après_BS, prop_after_BS[1]+prop_after_BS[3])
      prop_V_F_avant_BS <- append(prop_V_F_avant_BS, prop_before_BS[2]+prop_before_BS[4])
      prop_V_F_après_BS <- append(prop_V_F_après_BS, prop_after_BS[2]+prop_after_BS[4])
      prop_A_avant_BS <- append(prop_A_avant_BS, prop_before_BS[1])
      prop_R_avant_BS <- append(prop_R_avant_BS, prop_before_BS[3])
      prop_V_avant_BS <- append(prop_V_avant_BS, prop_before_BS[4])
      prop_F_avant_BS <- append(prop_F_avant_BS, prop_before_BS[2])
      prop_A_après_BS <- append(prop_A_après_BS, prop_after_BS[1])
      prop_R_après_BS <- append(prop_R_après_BS, prop_after_BS[3])
      prop_V_après_BS <- append(prop_V_après_BS, prop_after_BS[4])
      prop_F_après_BS <- append(prop_F_après_BS, prop_after_BS[2])
    }}
  
  if (apparition_JP[1] != 100){
    for (l in 1:length(apparition_JP)) {
      before = four_choice_i[as.integer(apparition_JP[l]-20):apparition_JP[l]]
      after = four_choice_i[apparition_JP[l]:as.integer(apparition_JP[l]+19)]
      prop_before_JP = table(before)/length(before)
      prop_after_JP = table(after)/length(after)
      prop_A_R_avant_JP <- append(prop_A_R_avant_JP, prop_before_JP[1]+prop_before_JP[3])
      prop_A_R_après_JP <- append(prop_A_R_après_JP, prop_after_JP[1]+prop_after_JP[3])
      prop_A_V_avant_JP <- append(prop_A_V_avant_JP, prop_before_JP[1]+prop_before_JP[4])
      prop_A_V_après_JP <- append(prop_A_V_après_JP, prop_after_JP[1]+prop_after_JP[4])
      prop_V_F_avant_JP <- append(prop_V_F_avant_JP, prop_before_JP[2]+prop_before_JP[4])
      prop_V_F_après_JP <- append(prop_V_F_après_JP, prop_after_JP[2]+prop_after_JP[4])
      prop_A_avant_JP <- append(prop_A_avant_JP, prop_before_JP[1])
      prop_R_avant_JP <- append(prop_R_avant_JP, prop_before_JP[3])
      prop_V_avant_JP <- append(prop_V_avant_JP, prop_before_JP[4])
      prop_F_avant_JP <- append(prop_F_avant_JP, prop_before_JP[2])
      prop_A_après_JP <- append(prop_A_après_JP, prop_after_JP[1])
      prop_R_après_JP <- append(prop_R_après_JP, prop_after_JP[3])
      prop_V_après_JP <- append(prop_V_après_JP, prop_after_JP[4])
      prop_F_après_JP <- append(prop_F_après_JP, prop_after_JP[2])
    }}
}

# unlist 
# BS
prop_A_R_avant_BS = unlist(prop_A_R_avant_BS)
prop_A_R_après_BS = unlist(prop_A_R_après_BS)
prop_V_F_avant_BS = unlist(prop_V_F_avant_BS)
prop_V_F_après_BS = unlist(prop_V_F_après_BS)
prop_A_avant_BS = unlist(prop_A_avant_BS)
prop_R_avant_BS = unlist(prop_R_avant_BS)
prop_V_avant_BS = unlist(prop_V_avant_BS)
prop_F_avant_BS = unlist(prop_F_avant_BS)
prop_A_après_BS = unlist(prop_A_après_BS)
prop_R_après_BS = unlist(prop_R_après_BS)
prop_V_après_BS = unlist(prop_V_après_BS)
prop_F_après_BS = unlist(prop_F_après_BS)
# JP
prop_A_R_avant_JP = unlist(prop_A_R_avant_JP)
prop_A_R_après_JP = unlist(prop_A_R_après_JP)
prop_A_V_avant_JP = unlist(prop_A_V_avant_JP)
prop_A_V_après_JP = unlist(prop_A_V_après_JP)
prop_V_F_avant_JP = unlist(prop_V_F_avant_JP)
prop_V_F_après_JP = unlist(prop_V_F_après_JP)
prop_A_avant_JP = unlist(prop_A_avant_JP)
prop_R_avant_JP = unlist(prop_R_avant_JP)
prop_V_avant_JP = unlist(prop_V_avant_JP)
prop_F_avant_JP = unlist(prop_F_avant_JP)
prop_A_après_JP = unlist(prop_A_après_JP)
prop_R_après_JP = unlist(prop_R_après_JP)
prop_V_après_JP = unlist(prop_V_après_JP)
prop_F_après_JP = unlist(prop_F_après_JP)


## plot carré des données 
# pour BS
par(mfrow= c(1,2))
plot(prop_A_R_avant_BS,prop_A_R_après_BS,col="firebrick",pch = "+",cex=2,xlim=c(0,1),ylim=c(0,1), xlab = "prop(A)+prop(R) before BS", ylab = "prop(A)+prop(R) after BS")
abline(a = 0, b = 1, col = "gray")
plot(prop_V_F_avant_BS,prop_V_F_après_BS,col="firebrick",pch = "+",cex=2,xlim=c(0,1),ylim=c(0,1), xlab = "prop(V)+prop(F) before BS", ylab = "prop(V)+prop(F) after BS")
abline(a = 0, b = 1, col = "gray")
par(mfrow= c(1,1))
# pour JP 
par(mfrow= c(2,2))
plot(prop_A_R_avant_JP,prop_A_R_après_JP,col="forestgreen",pch = "+",cex=2,xlim=c(0,1),ylim=c(0,1), xlab = "prop(A)+prop(R) before JP", ylab = "prop(A)+prop(R) after JP")
abline(a = 0, b = 1, col = "gray")
plot(prop_A_V_avant_JP,prop_A_V_après_JP,col="forestgreen",pch = "+",cex=2,xlim=c(0,1),ylim=c(0,1), xlab = "prop(A)+prop(V) before JP", ylab = "prop(A)+prop(V) after JP")
abline(a = 0, b = 1, col = "gray")
plot(prop_V_F_avant_JP,prop_V_F_après_JP,col="forestgreen",pch = "+",cex=2,xlim=c(0,1),ylim=c(0,1), xlab = "prop(V)+prop(F) before JP", ylab = "prop(V)+prop(F) after JP")
abline(a = 0, b = 1, col = "gray")
par(mfrow= c(1,1))
# pour toutes les options BS et JP 
par(mfrow= c(2,4))
plot(prop_A_avant_BS,prop_A_après_BS,col="firebrick",pch = "+",cex=2,xlim=c(0,1),ylim=c(0,1), xlab = "prop(A) before BS", ylab = "prop(A) after BS")
abline(a = 0, b = 1, col = "gray")
plot(prop_R_avant_BS,prop_R_après_BS,col="firebrick",pch = "+",cex=2,xlim=c(0,1),ylim=c(0,1), xlab = "prop(R) before BS", ylab = "prop(R) after BS")
abline(a = 0, b = 1, col = "gray")
plot(prop_V_avant_BS,prop_V_après_BS,col="firebrick",pch = "+",cex=2,xlim=c(0,1),ylim=c(0,1), xlab = "prop(V) before BS", ylab = "prop(V) after BS")
abline(a = 0, b = 1, col = "gray")
plot(prop_F_avant_BS,prop_F_après_BS,col="firebrick",pch = "+",cex=2,xlim=c(0,1),ylim=c(0,1), xlab = "prop(F) before BS", ylab = "prop(F) after BS")
abline(a = 0, b = 1, col = "gray")
plot(prop_A_avant_JP,prop_A_après_JP,col="forestgreen",pch = "+",cex=2,xlim=c(0,1),ylim=c(0,1), xlab = "prop(A) before JP", ylab = "prop(A) after JP")
abline(a = 0, b = 1, col = "gray")
plot(prop_R_avant_JP,prop_R_après_JP,col="forestgreen",pch = "+",cex=2,xlim=c(0,1),ylim=c(0,1), xlab = "prop(R) before JP", ylab = "prop(R) after JP")
abline(a = 0, b = 1, col = "gray")
plot(prop_V_avant_JP,prop_V_après_JP,col="forestgreen",pch = "+",cex=2,xlim=c(0,1),ylim=c(0,1), xlab = "prop(V) before JP", ylab = "prop(V) after JP")
abline(a = 0, b = 1, col = "gray")
plot(prop_F_avant_JP,prop_F_après_JP,col="forestgreen",pch = "+",cex=2,xlim=c(0,1),ylim=c(0,1), xlab = "prop(F) before JP", ylab = "prop(F) after JP")
abline(a = 0, b = 1, col = "gray")
par(mfrow= c(1,1))
# tests stats
wilcox.test(prop_A_R_avant_BS, prop_A_R_après_BS, paired = TRUE)
wilcox.test(prop_V_F_avant_BS, prop_V_F_après_BS, paired = TRUE)
wilcox.test(prop_A_R_avant_JP, prop_A_R_après_JP, paired = TRUE)
wilcox.test(prop_A_V_avant_JP, prop_A_V_après_JP, paired = TRUE)
wilcox.test(prop_V_F_avant_JP, prop_V_F_après_JP, paired = TRUE)
wilcox.test(prop_A_avant_BS,prop_A_après_BS, paired = TRUE)
wilcox.test(prop_R_avant_BS,prop_R_après_BS, paired = TRUE)
wilcox.test(prop_V_avant_BS,prop_V_après_BS, paired = TRUE)
wilcox.test(prop_F_avant_BS,prop_F_après_BS, paired = TRUE)
wilcox.test(prop_A_avant_JP,prop_A_après_JP, paired = TRUE)
wilcox.test(prop_R_avant_JP,prop_R_après_JP, paired = TRUE)
wilcox.test(prop_V_avant_JP,prop_V_après_JP, paired = TRUE)
wilcox.test(prop_F_avant_JP,prop_F_après_JP, paired = TRUE)


####### diversification 
# BS
diversification_avant_BS = list()
diversification_après_BS = list()
# JP
diversification_avant_JP = list()
diversification_après_JP = list()

apparition_BS <- rep(0,3)
apparition_JP <- rep(0,3)
data = data_fourchoices_2
n = 200
for (i in unique(data$id)) {
  apparition_BS = extract_REE(i, data, -3000, 10 , 1)
  apparition_JP = extract_REE(i, data, 3000, 10, 100)
  print(i)
  print(apparition_BS)
  print(apparition_JP)
  four_choice_i = data[data$id == i , ]$button_name
  
  if (apparition_BS[1] != 1){
    for (k in 1:length(apparition_BS)) {
      before = four_choice_i[as.integer(apparition_BS[k]-10):apparition_BS[k]]
      after = four_choice_i[apparition_BS[k]:as.integer(apparition_BS[k]+9)]
      prop_before_BS = table(before)/length(before)
      prop_after_BS = table(after)/length(after)
      diversification_avant_BS <- append(diversification_avant_BS, diversification.i(prop_before_BS))
      diversification_après_BS <- append(diversification_après_BS, diversification.i(prop_after_BS))
    }}
  
  if (apparition_JP[1] != 100){
    for (l in 1:length(apparition_JP)) {
      llabels_JP <- append(llabels_JP, i)
      before = four_choice_i[as.integer(apparition_JP[l]-10):apparition_JP[l]]
      after = four_choice_i[apparition_JP[l]:as.integer(apparition_JP[l]+9)]
      prop_before_JP = table(before)/length(before)
      prop_after_JP = table(after)/length(after)
      diversification_avant_JP <- append(diversification_avant_JP, diversification.i(prop_before_JP))
      diversification_après_JP <- append(diversification_après_JP, diversification.i(prop_after_JP))
    }}
}

# unlist 
# BS
diversification_avant_BS = unlist(diversification_avant_BS)
diversification_après_BS = unlist(diversification_après_BS)
# JP
diversification_avant_JP = unlist(diversification_avant_JP)
diversification_après_JP = unlist(diversification_après_JP)

## plot carré des données 
# pour BS
par(mfrow= c(1,2))
plot(diversification_avant_BS,diversification_après_BS,col="firebrick",pch = "+",cex=2,xlim=c(1,4),ylim=c(1,4), xlab = "diversification score before BS", ylab = "diversification score after BS")
abline(a = 0, b = 1, col = "gray")
plot(diversification_avant_JP,diversification_après_JP,col="forestgreen",pch = "+",cex=2,xlim=c(1,4),ylim=c(1,4), xlab = "diversification score before JP", ylab = "diversification score after JP")
abline(a = 0, b = 1, col = "gray")
par(mfrow= c(1,1))

wilcox.test(diversification_avant_BS,diversification_après_BS, paired = TRUE)
mean(diversification_avant_BS)
mean(diversification_après_BS)
wilcox.test(diversification_avant_JP,diversification_après_JP, paired = TRUE)
mean(diversification_avant_JP)
mean(diversification_après_JP)


#### en fonction des trials par participant
plot_choices_around_ree <- function(data, n_clics) {
  button_names <- levels(data$button_name) # Récupère les noms des boutons
  
  for (i in unique(data$id)) {
    # Obtenez les positions des JP et des BS
    positions_JP <- extract_REE(i, data, 3000, n_clics, 100)
    positions_BS <- extract_REE(i, data, -3000, n_clics, 1)
    
    # Trace pour les JP
    if (!(length(positions_JP) == 1 && positions_JP[1] == 100)) {
      plot(NULL, xlim = c(-n_clics, n_clics), ylim = c(1, length(button_names)), 
           xlab = "Trials (relative to REE)", ylab = "Button Name",
           main = paste("Choices around JP for participant", i),
           yaxt = "n")
      axis(2, at = 1:length(button_names), labels = button_names)
      #abline(v = 0, col = "green", lwd = 2, lty = 2)
      print(i)
      for (pos in positions_JP) {
        print(pos)
        around_ree <- get_temp_series_around_ree(pos, i, data, n_clics)[[1]]
        print(around_ree)
        lines(seq(-n_clics, n_clics), match(around_ree, button_names), col = "black", lwd = 2)
      }
    }
    
    # Trace pour les BS
    if (!(length(positions_BS) == 1 && positions_BS[1] == 1)) {
      plot(NULL, xlim = c(-n_clics, n_clics), ylim = c(1, length(button_names)), 
           xlab = "Trials (relative to REE)", ylab = "Button Name",
           main = paste("Choices around BS for participant", i),
           yaxt = "n")
      axis(2, at = 1:length(button_names), labels = button_names)
      #abline(v = 0, col = "red", lwd = 2, lty = 2)
      print(i)
      for (pos in positions_BS) {
        print(pos)
        around_ree <- get_temp_series_around_ree(pos, i, data, n_clics)[[1]]
        print(around_ree)
        lines(seq(-n_clics, n_clics), match(around_ree, button_names), col = "black", lwd = 2)
      }
    }
  }
}

plot_choices_around_ree(data_fourchoices_1, 20)

## scatter plots par options 
scatter_proportions_separated_by_button <- function(data, n_clics) {
  proportion_data <- data.frame()
  test_results_all <- data.frame()  # Pour stocker les résultats des tests
  
  # Construction des données pour proportions avant/après
  for (i in unique(data$id)) {
    positions_JP <- extract_REE(i, data, 3000, n_clics, 100)
    positions_BS <- extract_REE(i, data, -3000, n_clics, 1)
    
    if (!(length(positions_JP) == 1 && positions_JP[1] == 100)) {
      for (pos in positions_JP) {
        temp_series <- get_temp_series_around_ree(pos, i, data, n_clics)[[1]]
        before <- table(factor(temp_series[1:n_clics], levels = levels(data$button_name)))
        after <- table(factor(temp_series[(n_clics + 1):(2 * n_clics)], levels = levels(data$button_name)))
        
        proportion_data <- rbind(proportion_data, data.frame(
          Participant = i,
          REE_Type = "JP",
          Button = names(before),
          Before = as.numeric(before / sum(before)),
          After = as.numeric(after / sum(after))
        ))
      }
    }
    
    if (!(length(positions_BS) == 1 && positions_BS[1] == 1)) {
      for (pos in positions_BS) {
        temp_series <- get_temp_series_around_ree(pos, i, data, n_clics)[[1]]
        before <- table(factor(temp_series[1:n_clics], levels = levels(data$button_name)))
        after <- table(factor(temp_series[(n_clics + 1):(2 * n_clics)], levels = levels(data$button_name)))
        
        proportion_data <- rbind(proportion_data, data.frame(
          Participant = i,
          REE_Type = "BS",
          Button = names(before),
          Before = as.numeric(before / sum(before)),
          After = as.numeric(after / sum(after))
        ))
      }
    }
  }
  
  proportion_data <- na.omit(proportion_data)
  proportion_data <- proportion_data[proportion_data$Before >= 0 & proportion_data$After >= 0, ]
  
  for (button in levels(data$button_name)) {
    button_data <- proportion_data[proportion_data$Button == button, ]
    
    # Calcul des tests de Wilcoxon
    test_results <- button_data %>%
      group_by(REE_Type) %>%
      summarise(
        Statistic = ifelse(
          length(unique(Before - After)) > 1,
          wilcox.test(Before, After, paired = TRUE, exact = FALSE)$statistic,
          NA
        ),
        p_value = ifelse(
          length(unique(Before - After)) > 1,
          wilcox.test(Before, After, paired = TRUE, exact = FALSE)$p.value,
          NA
        ),
        n = n()
      ) %>%
      mutate(significance = case_when(
        !is.na(p_value) & p_value < 0.001 ~ "***",
        !is.na(p_value) & p_value < 0.01 ~ "**",
        !is.na(p_value) & p_value < 0.05 ~ "*",
        TRUE ~ "ns"
      ))
    
    test_results <- test_results %>% mutate(Button = button)
    test_results_all <- rbind(test_results_all, test_results)
    
    melted_data <- reshape2::melt(button_data, id.vars = c("Participant", "REE_Type", "Button"), 
                                  variable.name = "Time", value.name = "Proportion")
    melted_data$REE_Type <- factor(melted_data$REE_Type, levels = c("BS", "JP"))
    
    # Position des annotations : ajustée au-dessus de l'axe des abscisses
    test_results_annot <- test_results %>%
      mutate(x = 1.5, y = 1.05)  # Positionnement au-dessus de l'axe X
    
    # Scatter plot
    plot <- ggplot(melted_data, aes(x = Time, y = Proportion, color = REE_Type, group = interaction(Participant, REE_Type))) +
      geom_point(size = 3) +
      geom_line() +
      facet_wrap(~ REE_Type, scales = "free_y") +
      scale_y_continuous(limits = c(0, 1)) +
      labs(title = paste("Proportions around REE for Button:", button),
           x = "Time relative to REE (Before/After)", 
           y = "Proportion of choices") +
      scale_color_manual(values = c("JP" = "forestgreen", "BS" = "darkred")) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Centrer le titre
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 13),
            legend.title = element_text(size = 12)) +
      geom_text(data = test_results_annot, aes(x = x, y = y, label = paste0("p = ", round(p_value, 3), " ", significance)),
                inherit.aes = FALSE, color = "black", size = 3, na.rm = TRUE)
    
    print(plot)
  }
  
  return(test_results_all)
}

scatter_proportions_separated_by_button(data_fourchoices_1, 20)
scatter_proportions_separated_by_button(data_fourchoices_2, 20)
# mettre les faire test stats rank aparié wilconxon 


# boxplot + scatter plot 
library(dplyr)
library(ggplot2)
library(reshape2)

scatter_proportions_separated_by_button <- function(data, n_clics) {
  proportion_data <- data.frame()
  test_results_all <- data.frame()  # Pour stocker les résultats des tests
  
  # Construction des données pour proportions avant/après
  for (i in unique(data$id)) {
    positions_JP <- extract_REE(i, data, 3000, n_clics, 100)
    positions_BS <- extract_REE(i, data, -3000, n_clics, 1)
    
    # JP
    if (!(length(positions_JP) == 1 && positions_JP[1] == 100)) {
      for (pos in positions_JP) {
        temp_series <- get_temp_series_around_ree(pos, i, data, n_clics)[[1]]
        before <- table(factor(temp_series[1:n_clics], levels = levels(data$button_name)))
        after <- table(factor(temp_series[(n_clics + 1):(2 * n_clics)], levels = levels(data$button_name)))
        
        proportion_data <- rbind(proportion_data, data.frame(
          Participant = i,
          REE_Type = "JP",
          Button = names(before),
          Time = rep(c("Before", "After"), each = length(before)),  # "Before" à gauche et "After" à droite
          Proportion = c(as.numeric(before / sum(before)), as.numeric(after / sum(after)))  # "Before" d'abord, puis "After"
        ))
      }
    }
    
    # BS
    if (!(length(positions_BS) == 1 && positions_BS[1] == 1)) {
      for (pos in positions_BS) {
        temp_series <- get_temp_series_around_ree(pos, i, data, n_clics)[[1]]
        before <- table(factor(temp_series[1:n_clics], levels = levels(data$button_name)))
        after <- table(factor(temp_series[(n_clics + 1):(2 * n_clics)], levels = levels(data$button_name)))
        
        proportion_data <- rbind(proportion_data, data.frame(
          Participant = i,
          REE_Type = "BS",
          Button = names(before),
          Time = rep(c("Before", "After"), each = length(before)),  # "Before" à gauche et "After" à droite
          Proportion = c(as.numeric(before / sum(before)), as.numeric(after / sum(after)))  # "Before" d'abord, puis "After"
        ))
      }
    }
  }
  
  proportion_data <- na.omit(proportion_data)
  
  # Pour chaque bouton, on crée un graphique
  for (button in levels(data$button_name)) {
    button_data <- proportion_data[proportion_data$Button == button, ]
    
    # Calcul du test de Wilcoxon pour "Before" et "After"
    test_results <- button_data %>%
      group_by(REE_Type) %>%
      summarise(
        p_value = wilcox.test(Proportion[Time == "Before"], Proportion[Time == "After"], paired = TRUE, exact = FALSE)$p.value
      ) %>%
      mutate(significance = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        p_value < 0.05 ~ "*",
        TRUE ~ "ns"
      ))
    
    # Affichage des résultats du test de Wilcoxon dans la console
    print(paste("Results for Button:", button))
    print(test_results)
    
    # Annotations des résultats de test
    test_results_annot <- test_results %>%
      mutate(x = 1.5, y = 1.05, label = paste0("p = ", round(p_value, 3), " ", significance))
    
    # Scatter plot et boxplot combinés
    plot <- ggplot(button_data, aes(x = Time, y = Proportion, color = REE_Type)) +
      # Boxplot sans croix, utilisant la moyenne
      geom_boxplot(aes(group = Time), alpha = 0.4, outlier.shape = NA, color = "black") +
      # Points alignés verticalement
      geom_point(aes(group = Participant), size = 3, position = position_dodge(width = 0.1)) +
      # Lignes fines reliant les points
      geom_line(aes(group = Participant, color = REE_Type), size = 0.5, alpha = 0.7) +
      facet_wrap(~ REE_Type) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(title = paste("Proportions around REE for Button:", button),
           x = "Time relative to REE (Before/After)", 
           y = "Proportion of choices") +
      scale_color_manual(values = c("JP" = "forestgreen", "BS" = "darkred")) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 13),
            legend.title = element_text(size = 12)) +
      # Affichage des résultats du test de Wilcoxon
      geom_text(data = test_results_annot, aes(x = x, y = y, label = label),
                inherit.aes = FALSE, color = "black", size = 4, na.rm = TRUE)
    
    print(plot)
  }
  
  return(test_results_all)
}

scatter_proportions_separated_by_button(data_fourchoices_1, 20)

##### pour la diversification 
scatter_diversification <- function(data, n_clics) {
  proportion_data <- data.frame()
  test_results_all <- data.frame()  # Pour stocker les résultats des tests
  
  # Construction des données pour proportions avant/après
  for (i in unique(data$id)) {
    positions_JP <- extract_REE(i, data, 3000, n_clics, 100)
    positions_BS <- extract_REE(i, data, -3000, n_clics, 1)
    
    if (!(length(positions_JP) == 1 && positions_JP[1] == 100)) {
      for (pos in positions_JP) {
        temp_series <- get_temp_series_around_ree(pos, i, data, n_clics)[[1]]
        before <- diversification.i(temp_series[1:n_clics])
        after <- diversification.i(temp_series[(n_clics + 1):(2 * n_clics)])
        
        proportion_data <- rbind(before,after, data.frame(
          Participant = i,
          REE_Type = "JP",
          Button = names(before),
          Before = as.numeric(before),
          After = as.numeric(after)
        ))
      }
    }
    
    if (!(length(positions_BS) == 1 && positions_BS[1] == 1)) {
      for (pos in positions_BS) {
        temp_series <- get_temp_series_around_ree(pos, i, data, n_clics)[[1]]
        before <- diversification.i(temp_series[1:n_clics])
        after <- diversification.i(temp_series[(n_clics + 1):(2 * n_clics)])
        
        proportion_data <- rbind(before,after, data.frame(
          Participant = i,
          REE_Type = "JP",
          Button = names(before),
          Before = as.numeric(before),
          After = as.numeric(after)
        ))
      }
    }
  }
  
  proportion_data <- na.omit(proportion_data)
  proportion_data <- proportion_data[proportion_data$Before >= 0 & proportion_data$After >= 0, ]

    
    # Calcul des tests de Wilcoxon
  test_results <- proportion_data %>%
    group_by(REE_Type) %>%
    summarise(Statistic = ifelse(
        length(unique(Before - After)) > 1,
        wilcox.test(Before, After, paired = TRUE, exact = FALSE)$statistic,NA),
        p_value = ifelse(length(unique(Before - After)) > 1, wilcox.test(Before, After, paired = TRUE, exact = FALSE)$p.value,NA),
        n = n() )
    %>%
    mutate(significance = case_when(
      !is.na(p_value) & p_value < 0.001 ~ "***",
      !is.na(p_value) & p_value < 0.01 ~ "**",
      !is.na(p_value) & p_value < 0.05 ~ "*",
      TRUE ~ "ns"))
    
    test_results_all <- rbind(test_results_all, test_results)
    
    melted_data <- reshape2::melt(button_data, id.vars = c("Participant", "REE_Type"), 
                                  variable.name = "Time", value.name = "Proportion")
    melted_data$REE_Type <- factor(melted_data$REE_Type, levels = c("BS", "JP"))
    
    # Position des annotations : ajustée au-dessus de l'axe des abscisses
    test_results_annot <- test_results %>%
      mutate(x = 1.5, y = 1.05)  # Positionnement au-dessus de l'axe X
    
    # Scatter plot
    plot <- ggplot(melted_data, aes(x = Time, y = Proportion, color = REE_Type, group = interaction(Participant, REE_Type))) +
      geom_point(size = 3) +
      geom_line() +
      facet_wrap(~ REE_Type, scales = "free_y") +
      scale_y_continuous(limits = c(0, 1)) +
      labs(title = paste("Diversification score before and after REE"),
           x = "Time relative to REE (Before/After)", 
           y = "Diversification score") +
      scale_color_manual(values = c("JP" = "forestgreen", "BS" = "darkred")) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Centrer le titre
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 13),
            legend.title = element_text(size = 12)) +
      geom_text(data = test_results_annot, aes(x = x, y = y, label = paste0("p = ", round(p_value, 3), " ", significance)),
                inherit.aes = FALSE, color = "black", size = 3, na.rm = TRUE)
    
    print(plot)
  
  return(test_results_all)
}

scatter_proportions_separated_by_button(data_fourchoices_1, 20)
