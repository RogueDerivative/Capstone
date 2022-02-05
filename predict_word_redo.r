library(tidyverse)
library(tidytext)

source("uni_gram.r")
source("bi_gram.r")
source("tri_gram.r")
source("four_gram.r")
source("five_gram.r")

user_input <- "dancing in the"
last_word <- function(user_input) {
    my_num <- 1
    alpha <- 1
    row_count <- 0
    pred_word <- tibble()
    five_pred_word <- tibble()
    five_pred_word_A <- tibble()
    five_pred_word_B <- tibble()
    four_pred_word <- tibble()
    four_pred_word_A <- tibble()
    four_pred_word_B <- tibble()
    tri_pred_word <- tibble()
    tri_pred_word_A <- tibble()
    tri_pred_word_B <- tibble()
    bi_pred_word <- tibble()
    bi_pred_word_A <- tibble()
    bi_pred_word_B <- tibble()
    my_EOS <- tibble()
    user_input <- tibble(text=user_input)
    user_gram <- user_input %>%
        unnest_tokens(word,text) %>%
        filter(!str_detect(word,"[[:punct:]]")) %>%
        filter(!str_detect(word,"\\d"))
    empty_vector <- tibble(word = c("NA","NA","NA","NA","NA"))

    if(nrow(user_gram) < 4){
        user_gram <- bind_rows(empty_vector,user_gram)
    }
    word_n <- user_gram[nrow(user_gram),]
    n_minus_one_word <- user_gram[nrow(user_gram)-1,]
    n_minus_two_word <- user_gram[nrow(user_gram)-2,]
    n_minus_three_word <- user_gram[nrow(user_gram)-3,]

    while(my_num > 0) {
        # five grams
        five_gram_A_qBO <- function(n_minus_three_word,n_minus_two_word,
                                    n_minus_one_word,word_n,my_EOS,alpha) {
            five_gram_A <- five_gram %>%
                filter(word1==n_minus_three_word$word &
                    word2==n_minus_two_word$word &
                    word3==n_minus_one_word$word & 
                    word4==word_n$word)%>%
                mutate(dis_count=n-0.5)
            my_EOS <<- tibble(EOS=five_gram_A$EOS)
            five_gram_A <- five_gram_A %>%
                mutate(qBO=dis_count/sum(n))
            five_pred_word_A <<- five_gram_A  %>%
                select(EOS,qBO)%>%
                distinct(EOS,.keep_all = TRUE)
            
            alpha <<- as.numeric((1-sum(five_gram_A$dis_count)/sum(five_gram_A$n)))
        }
        five_gram_A_qBO(n_minus_three_word,n_minus_two_word,
                                    n_minus_one_word,word_n,my_EOS,alpha)
        five_gram_B_qBO <- function(n_minus_two_word,n_minus_one_word,word_n,
                                    my_EOS,alpha) {
            five_gram_B <- four_gram %>%
                filter(!EOS %in% my_EOS) 
            five_gram_B <- five_gram_B %>%
                mutate(qBO=n/sum(n))
            five_gram_B$qBO <- alpha*five_gram_B$qBO
            five_pred_word_B <<- five_gram_B  %>%
                select(EOS,qBO)%>%
                distinct(EOS,.keep_all = TRUE)
        }
        five_gram_B_qBO(n_minus_two_word,n_minus_one_word,word_n,my_EOS,alpha)
        
        five_pred_word <- bind_rows(five_pred_word_A,five_pred_word_B)
        
        # four grams
        four_gram_A_qBO <- function(n_minus_two_word,n_minus_one_word,
                                    word_n,my_EOS,alpha) {
            four_gram_A <- four_gram %>%
                filter(word1==n_minus_two_word$word &
                    word2==n_minus_one_word$word & 
                    word3==word_n$word)%>%
                mutate(dis_count=n-0.5)
            my_EOS <<- tibble(EOS=four_gram_A$EOS)
            four_gram_A <- four_gram_A %>%
                mutate(qBO=dis_count/sum(n))
            four_pred_word_A <<- four_gram_A  %>%
                select(EOS,qBO)%>%
                distinct(EOS,.keep_all = TRUE)
            
            alpha <<- as.numeric((1-sum(four_gram_A$dis_count)/sum(four_gram_A$n)))
        }
        four_gram_A_qBO(n_minus_two_word,n_minus_one_word,
                        word_n,my_EOS,alpha)
        four_gram_B_qBO <- function(n_minus_one_word,word_n,my_EOS,alpha) {
            four_gram_B <- tri_gram %>%
                filter(!EOS %in% my_EOS) 
            four_gram_B <- four_gram_B %>%
                mutate(qBO=n/sum(n))
            four_gram_B$qBO <- alpha*four_gram_B$qBO
            four_pred_word_B <<- four_gram_B  %>%
                select(EOS,qBO)%>%
                distinct(EOS,.keep_all = TRUE)
        }
        
        four_gram_B_qBO(n_minus_one_word,word_n,my_EOS,alpha)
        
        four_pred_word <- bind_rows(four_pred_word_A,four_pred_word_B)
        
        # tri grams
        tri_gram_A_qBO <- function(n_minus_one_word,word_n,my_EOS,alpha) {
            tri_gram_A <- tri_gram %>%
                filter(word1==n_minus_one_word$word & 
                           word2==word_n$word)%>%
                mutate(dis_count=n-0.5)
            my_EOS <<- tibble(EOS=tri_gram_A$EOS)
            tri_gram_A <- tri_gram_A %>%
                mutate(qBO=dis_count/sum(n))
            tri_pred_word_A <<- tri_gram_A  %>%
                select(EOS,qBO)%>%
                distinct(EOS,.keep_all = TRUE)
            
            alpha <<- as.numeric((1-sum(tri_gram_A$dis_count)/sum(tri_gram_A$n)))
        }
        
        tri_gram_A_qBO(n_minus_one_word,word_n,my_EOS,alpha)
        
        tri_gram_B_qBO <- function(word_n,my_EOS,alpha) {
            tri_gram_B <- bi_gram %>%
                filter(!EOS %in% my_EOS) 
            tri_gram_B <- tri_gram_B %>%
                mutate(qBO=n/sum(n))
            tri_gram_B$qBO <- alpha*tri_gram_B$qBO
            tri_pred_word_B <<- tri_gram_B  %>%
                select(EOS,qBO)%>%
                distinct(EOS,.keep_all = TRUE)
        }
        
        tri_gram_B_qBO(word_n,my_EOS,alpha)
        
        tri_pred_word <- bind_rows(tri_pred_word_A,tri_pred_word_B)
        
        # bi grams
        bi_gram_A_qBO <- function(word_n,alpha) {
            bi_gram_A <- bi_gram %>%
                filter(word1==word_n$word)%>%
                mutate(dis_count=n-0.5)
            my_EOS <<- bi_gram_A$EOS
            bi_gram_A <- bi_gram_A %>%
                mutate(qBO=dis_count/sum(n))
            bi_pred_word_A <<- bi_gram_A  %>%
                select(EOS,qBO)%>%
                distinct(EOS,.keep_all = TRUE)
            
            alpha <<- as.numeric(alpha*(1-sum(bi_gram_A$dis_count)/sum(bi_gram_A$n)))
            
        }
        
        bi_gram_A_qBO(word_n,alpha)
        
        bi_gram_B_qBO <- function(word_n,my_EOS,alpha){
            bi_gram_B <- bi_gram %>%
                filter(!EOS %in% my_EOS) %>%
                mutate(qBO=n/sum(n))
            bi_gram_B$qBO <- bi_gram_B$qBO*alpha
            bi_pred_word_B <<- bi_gram_B  %>%
                select(EOS,qBO)%>%
                distinct(EOS,.keep_all = TRUE)
        }
        
        bi_gram_B_qBO(word_n,my_EOS,alpha)
        
        bi_pred_word <- bind_rows(bi_pred_word_A,bi_pred_word_B)
        
        # word prediction table
        pred_word <- bind_rows(five_pred_word,
                               four_pred_word,
                               tri_pred_word,
                               bi_pred_word)
        
        # logical test 
        num_row <- nrow(pred_word)
        
        if(num_row>0){
            pred_word <- pred_word %>%
                arrange(desc(qBO)) %>%
                distinct(EOS,.keep_all = TRUE)%>%
                slice(1:3)
            my_num <-0
            }
        
        if(my_num ==0) {
            print(pred_word$EOS)
            break
        }
        
        pred_word <- "UNK"
        my_num <- 0
        if(my_num ==0) {
            print(pred_word$EOS)
            break
        }
        
    }
    
}

