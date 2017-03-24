function(input, output, clientData, session) {
  
  observeEvent(input$analysis, { 
    
    isolate({
      withProgress({
        setProgress(message = "분석중 ...")
        cachedResult(input$domainData)
        
        data_review()
        
        switch(input$domainData, 
               titanic={
                   taeIn_titanic()
                   taeHun_titanic()
                   #leeEarl_titanic()
                   #saeHyun_titanic()
                   #wonDoo_titanic()
               },
               animal={
                 taeIn_animal()
                 #taeHun_animal()
                 #leeEarl_animal()
                 #saeHyun_animal()
                 wonDoo_animal()
               },
               {
                 print('Choice domain data!')
               }
        )#End Switch
        
      })
    })
    
  })
  
  output$introduction <- renderUI({
  
    if(input$domainData == 'titanic'){
      
      #includeHTML("/srv/connect/apps/kaggle_shiny/kaggle_shiny_titanic.html")
      includeHTML("kaggle_shiny_titanic.html")
      
    }else if(input$domainData == 'animal'){
     
      #includeHTML("/srv/connect/apps/kaggle_shiny/kaggle_shiny_animal.html")
      includeHTML("kaggle_shiny/kaggle_shiny_animal.html")
      
    }
    
  })
  
  
  data_review <- function(){
    output$test_review <- DT::renderDataTable({test})
    
    output$train_review <- DT::renderDataTable({train})
    
    output$answer_review <- DT::renderDataTable({answer})
  }
  
  taeIn_titanic <- function(){
    
    test$Survived <- NA 
    
    full  <- rbind(train, test) # bind training & test data
    # check data
    str(full)
    
    # Grab title from passenger names
    full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
    
    # Show title counts by sex
    table(full$Sex, full$Title)
    
    # Titles with very low cell counts to be combined to "rare" level
    rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                    'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
    
    # Also reassign mlle, ms, and mme accordingly
    full$Title[full$Title == 'Mlle']        <- 'Miss' 
    full$Title[full$Title == 'Ms']          <- 'Miss'
    full$Title[full$Title == 'Mme']         <- 'Mrs' 
    full$Title[full$Title %in% rare_title]  <- 'Rare Title'
    
    output$taein1 <- renderTable({
      
      # Show title counts by sex again
      table(full$Sex, full$Title)
      
    })
    
    # Finally, grab surname from passenger name
    str(full$Name)
    full$Surname <- sapply(as.character(full$Name),  
                           function(x) strsplit(x, split = '[,.]')[[1]][1])
    
    cat(paste('We have <b>', nlevels(factor(full$Surname)), '</b> unique surnames. I would be interested to infer ethnicity based on surname --- another time.'))
    
    # Create a family size variable including the passenger themselves
    full$Fsize <- full$SibSp + full$Parch + 1
    
    # Create a family variable 
    full$Family <- paste(full$Surname, full$Fsize, sep='_')
    
    # Use ggplot2 to visualize the relationship between family size & survival
    output$taein2 <- renderPlot({
      
      ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
        geom_bar(stat='count', position='dodge') +
        scale_x_continuous(breaks=c(1:11)) +
        labs(x = 'Family Size')
      
    })
    
    
    # Discretize family size
    full$FsizeD[full$Fsize == 1] <- 'singleton'
    full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
    full$FsizeD[full$Fsize > 4] <- 'large'
    
    output$taein3 <- renderPlot({
      
      # Show family size by survival using a mosaic plot
      mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)
      
    })
    
    # This variable appears to have a lot of missing values
    full$Cabin[1:28]
    
    # The first character is the deck. For example:
    strsplit(full$Cabin[2], NULL)[[1]]
    
    # Create a Deck variable. Get passenger deck A - F:
    full$Deck<-factor(sapply(as.character(full$Cabin), function(x) strsplit(x, NULL)[[1]][1]))
    
    # Passengers 62 and 830 are missing Embarkment
    full[c(62, 830), 'Embarked']
    
    cat(paste('We will infer their values for **embarkment** based on present data that we can imagine may be relevant: **passenger class** and **fare**. We see that they paid<b> $', full[c(62, 830), 'Fare'][[1]][1], '</b>and<b> $', full[c(62, 830), 'Fare'][[1]][2], '</b>respectively and their classes are<b>', full[c(62, 830), 'Pclass'][[1]][1], '</b>and<b>', full[c(62, 830), 'Pclass'][[1]][2], '</b>. So from where did they embark?'))
    
    full[,'PassengerId']
    
    # Get rid of our missing passenger IDs
    embark_fare <- full %>%
      filter(PassengerId != 62 & PassengerId != 830)
    
    nrow(embark_fare)
    nrow(full)
    
    output$taein4 <- renderPlot({
      
      # Use ggplot2 to visualize embarkment, passenger class, & median fare
      ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
        geom_boxplot() +
        geom_hline(aes(yintercept=80), 
                   colour='red', linetype='dashed', lwd=2) +
        scale_y_continuous(labels=dollar_format())
      
    })
    
    full[c(62, 830), ]
    
    # Since their fare was $80 for 1st class, they most likely embarked from 'C'
    full$Embarked[c(62, 830)] <- 'C'
    
    # Show row 1044
    full[1044, ]
    
    output$taein5 <- renderPlot({
      
      ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
             aes(x = Fare)) +
        geom_density(fill = '#99d6ff', alpha=0.4) + 
        geom_vline(aes(xintercept=median(Fare, na.rm=T)),
                   colour='red', linetype='dashed', lwd=1) +
        scale_x_continuous(labels=dollar_format()) +
        theme_few()
      
    })
    
    # Replace missing fare value with median fare for class/embarkment
    full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)
    
    # Show number of missing Age values
    sum(is.na(full$Age))
    
    # Make variables factors into factors
    factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                     'Title','Surname','Family','FsizeD')
    
    full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))
    
    # Set a random seed
    set.seed(129)
    
    # Perform mice imputation, excluding certain less-than-useful variables:
    mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 
    
    # Save the complete output 
    mice_output <- complete(mice_mod)
    
    output$taein6 <- renderPlot({
      
      # Plot age distributions
      par(mfrow=c(1,2))
      hist(full$Age, freq=F, main='Age: Original Data', 
           col='darkgreen', ylim=c(0,0.04))
      hist(mice_output$Age, freq=F, main='Age: MICE Output', 
           col='lightgreen', ylim=c(0,0.04))
    })
    
    # Replace Age variable from the mice model.
    full$Age <- mice_output$Age
    
    # Show new number of missing Age values
    sum(is.na(full$Age))
    
    output$taein7 <- renderPlot({
      
      # First we'll look at the relationship between age & survival
      ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
        geom_histogram() + 
        # I include Sex since we know (a priori) it's a significant predictor
        facet_grid(.~Sex) + 
        theme_few()
      
    })
    
    # Create the column child, and indicate whether child or adult
    full$Child[full$Age < 18] <- 'Child'
    full$Child[full$Age >= 18] <- 'Adult'
    
    # Show counts
    table(full$Child, full$Survived)
    
    # Adding Mother variable
    full$Mother <- 'Not Mother'
    full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'
    
    # Show counts
    table(full$Mother, full$Survived)
    
    # Finish by factorizing our two new factor variables
    full$Child  <- factor(full$Child)
    full$Mother <- factor(full$Mother)
    
    md.pattern(full)
    full[c(891, 418),]
    
    # Split the data back into a train set and a test set
    train <- full[1:891,]
    test <- full[892:1309,]
    
    # Set a random seed
    set.seed(754)
    
    # Build the model (note: not all possible variables are used)
    rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                               Fare + Embarked + Title + 
                               FsizeD + Child + Mother,
                             data = train)
    
    output$taein8 <- renderPlot({
      
      # Show model error
      plot(rf_model, ylim=c(0,0.36))
      legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
      
    })
    
    # Get importance
    importance    <- importance(rf_model)
    varImportance <- data.frame(Variables = row.names(importance), 
                                Importance = round(importance[ ,'MeanDecreaseGini'],2))
    
    # Create a rank variable based on importance
    rankImportance <- varImportance %>%
      mutate(Rank = paste0('#',dense_rank(desc(importance))))
    
    output$taein9 <- renderPlot({
      
      # Use ggplot2 to visualize the relative importance of variables
      ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                                 y = Importance, fill = Importance)) +
        geom_bar(stat='identity') + 
        geom_text(aes(x = Variables, y = 0.5, label = Rank),
                  hjust=0, vjust=0.55, size = 4, colour = 'red') +
        labs(x = 'Variables') +
        coord_flip() + 
        theme_few()
      
    })
    
    full[,'Title']
    
    # Predict using the test set
    prediction <- predict(rf_model, test)
    
    # Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
    solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
    
    #실제 데이터로 예측률 확인
    answer<-answer[,2]
    answer[answer==1]<-"live"
    answer[answer==0]<-"dead"
    last.answer<-table(answer,prediction)
    
    #정답률
    accurate<-(last.answer[1,1]+last.answer[2,2])/sum(last.answer)
    
    
    output$taein10 <- renderTable({
      
      table(solution$PassengerID, solution$Survived, dnn = c('PassengerId', 'Survived'))
      
    })
    
    output$taein11 <- renderText({
      
      print(accurate)
      
    })
    
  }
  
  
  
  taeIn_animal <- function(){
    
    names(train)[1] = "ID"
    test$ID = as.character(test$ID)
    
    full = bind_rows(train, test)
    
    #나이 데이터를 일별로 처리
    age <- apply(full, 1, function(x){
      if( 1 == length(grep('day', x[8]))){
        
        substr(x[8], 1, 2)
        
      }else if( 1 == length(grep('week', x[8]))){
        
        as.numeric(substr(x[8], 1, 2)) * 7
        
      }else if( 1 == length(grep('month', x[8]))){
        
        as.numeric(substr(x[8], 1, 2)) * 30
        
      }else if( 1 == length(grep('year', x[8]))){
        
        as.numeric(substr(x[8], 1, 2)) * 365
        
      }else{
        
        NA
      }
    })
    
    full$AgeuponOutcome <- age
    
    Date_POSIXct <- ymd_hms(full$DateTime)
    full <- cbind(full, Date_POSIXct)
    
    full_lm <- lm(AgeuponOutcome ~ Date_POSIXct, data = full, na.action = na.omit)
    
    summary(full_lm)
    
    output$taein12 <- renderPlot({
      plot(AgeuponOutcome ~ Date_POSIXct, data = full)
      abline(reg=full_lm, col="red")
    })
    
    #age가 NA인 데이터 셋
    age_na <- full[which(is.na(age)),]
    predict <- predict(full_lm, newdata = age_na)
    class(predict)
    
    #age결측값 lm으로 대체
    full$AgeuponOutcome <- as.numeric(full$AgeuponOutcome)
    full[which(is.na(age)), 'AgeuponOutcome'] <- predict
    
    str(full)
    
    #chr형을 factor형으로 변환
    full$OutcomeType <- as.factor(full$OutcomeType)
    full$OutcomeSubtype <- as.factor(full$OutcomeSubtype)
    full$AnimalType <- as.factor(full$AnimalType)
    full$SexuponOutcome <- as.factor(full$SexuponOutcome)
    
    
    
    #이름이 있는 동물이랑 없는 동물로 사람에게 사랑을 받았는지 안받았는지 알수 있음(1:이름있음, 0:이름 없음)
    for(i in 1:length(full$Name)){
      if(full$Name[i] == ""){
        full$Name_factor[i] = 0
      }else{
        full$Name_factor[i] = 1
      }
    }
    
    full$Name_factor <- as.factor(full$Name_factor)
    str(full)
    
    #종에서 잡견들은 우성 유전자로(+1)
    breed_mix <- grep("Mix", full$Breed)
    
    #3대 천사견(푸들, 골든리트리버, 진돗개)(Poodle, Golden Retriever, Jindo)(+1)
    #3대 악마견(슈나우저, 코카스파니엘, 비글)(Schnauzer, Cocker Spaniel, Beagle)(-1)
    angel_dog <- c(grep("Poodle", full$Breed), grep("Golden Retriever", full$Breed), grep("Jindo", full$Breed))
    devil_dog <- c(grep("Schnauzer", full$Breed), grep("Cocker Spaniel", full$Breed), grep("Beagle", full$Breed))
    
    Breed_factor <- rep(0, NROW(full))
    full <- cbind(full, Breed_factor)
    full[breed_mix, "Breed_factor"] <- 1
    full[angel_dog, "Breed_factor"] <- full[angel_dog, "Breed_factor"] + 1
    full[devil_dog, "Breed_factor"] <- full[devil_dog, "Breed_factor"] - 1
    str(full)
    
    full$simpleColor <- sapply(full$Color, function(x){
      strsplit(x, split = '/')[[1]][1]
    })
    
    full$simpleColor<-as.factor(full$simpleColor)
    
    #모델링 변수들 타입에는 chr형이 오면 안된다
    #factor에 level이 53개 이상이면 랜덤포레스트를 진행 할수 없다
    library(randomForest)
    
    train = full[1:26729, ]
    test = full[26730:nrow(full), ]
    
    full_rf<-randomForest(OutcomeType ~ AnimalType + SexuponOutcome + AgeuponOutcome + Date_POSIXct + Breed_factor + Name_factor, data=train, ntree=500, importance=T, na.action = na.omit)
    
    importance(full_rf)
    
    output$taein13 <- renderPlot({
      varImpPlot(full_rf)
    })
    
    output$taein14 <- renderPlot({
      plot(full_rf, 'l')
    })
    
    predicted <- predict(full_rf, newdata=test)
    
    output$taein15 <- renderTable({
      solution<-data.table('ID'=test$ID, predicted)
      table(solution)
    })
  }
  
  
  
  taeHun_titanic <- function(){
    
    #라이브러리
    library(dplyr)
    library(randomForest)
    
    Survived<-rep(NA,418)
    test<-cbind(test[,1], Survived, test[,2:11])
    colnames(test)[1]<-c("PassengerId")
    
    #의미있는 데이터 추출과 변환
    train[train$Survived==1,2]<-"live"
    train[train$Survived==0,2]<-"dead"
    train<-transform(train,Survived= as.factor(Survived))
    titanic<-rbind(train,test)
    titanic.mean<-titanic[,c(2,3,5:8,10)]
    
    
    
    #결측 값처리
    
    sum(is.na(titanic.mean$Survived))#결측값 418개
    sum(is.na(titanic.mean$Pclass))#결측값 없음
    sum(is.na(titanic.mean$Sex))#결측값 없음
    sum(is.na(titanic.mean$Age))#결측값177개
    sum(is.na(titanic.mean$SibSp))#결측값 없음
    sum(is.na(titanic.mean$Parch))#결측값 없음
    sum(is.na(titanic.mean$Fare))#결측값 1개
    which(is.na(titanic.mean$Fare)==TRUE)#1044번째 index
    titanic.mean$Fare[1044]<-mean(titanic.mean$Fare,na.rm=TRUE)
    
    
    #단계선택법을 이용해서 age와 나머지 변수 회귀분석
    
    full<-lm(titanic.mean$Age~titanic.mean$Fare + titanic.mean$Pclass +titanic.mean$SibSp +titanic.mean$Parch,data=titanic.mean)
    null<-lm(titanic.mean$Age~1)
    relation<-step(null, direction = "both", scope=list(upper=full))
    summary(relation)
    equation<-relation$coefficients
    gradient<-as.vector(equation[-1])
    y<-as.vector(equation[1])
    age<-as.vector(as.matrix(titanic.mean[,c(2,5,6)])%*%gradient+y)
    
    #age 결측값에 수를 채워넣기
    boolean<-is.na(titanic.mean$Age)
    ex<-cbind(boolean,titanic.mean)
    
    for(i in 1:length(ex[,1])) {
      
      if(ex$boolean[i]==TRUE)
      {ex$Age[i]=age[i]
      }
      else
      {}
    }  
    
    #랜덤 포레스트 생성
    titanic.last<-ex[,-1]
    
    titanic.last$Sex <- as.factor(titanic.last$Sex)#태인 chr형으로 factor형으로 변환 
    
    titanic.rf<-randomForest(Survived~.,data=titanic.last,ntree=100,mytr=6,importance=T ,na.action =  na.omit )
    summary(titanic.rf)
    titanic.rf$importance
    
    output$taehun1 <- renderPlot({
      
      plot(titanic.rf,'l')
      
    })
    
    #TEST 데이터 예측
    speculate<-predict(titanic.rf,newdata=titanic.last[892:1309,])
    speculate<-as.vector(speculate)
    
    #실제 데이터로 예측률 확인
    answer<-answer[,2]
    answer[answer==1]<-"live"
    answer[answer==0]<-"dead"
    last.answer<-table(answer,speculate)
    
    #정답률
    accurate<-(last.answer[1,1]+last.answer[2,2])/sum(last.answer)
    
    output$taehun2 <- renderText({
      
      print(accurate)
      
    })
    
  }
  
  
  
  
  wonDoo_animal <- function(){
    
    
    
    names(train)[1] = "ID"
    
    test$ID = as.character(test$ID)
    full = bind_rows(train, test)
    
    outcomes = full[1:26729, ]
    outcomes = group_by(outcomes, AnimalType, OutcomeType)
    outcomes = summarise(outcomes, num_animals=n())
    
    output$wondoo1 <- renderPlot({
      
    #시각화
    ggplot(outcomes, aes(x=AnimalType, y=num_animals, fill=OutcomeType)) + 
      geom_bar(stat='identity', Position='fill', colour='black') +
      coord_flip() + 
      labs(y = 'Proportion of Animals',
           x = 'Animal',
           title = 'Outcomes: Cat & Dog') + 
      theme_few()
    })
      
      
    full$TimeValue = sapply(full$AgeuponOutcome, function(x){
      strsplit(x, split=" ")[[1]][1]
    })
    full$UnitofTime = sapply(full$AgeuponOutcome, function(x){
      strsplit(x, split=" ")[[1]][2]
    })
    
    
    full$TimeValue = as.numeric(full$TimeValue)
    full$UnitofTime = as.factor(full$UnitofTime)
    full$UnitofTime = gsub("s", "", full$UnitofTime)
    
    
    full$multiplier =  ifelse(full$UnitofTime=="day",1,
                              ifelse(full$UnitofTime=="week",7,
                                     ifelse(full$UnitofTime=="month",30,
                                            ifelse(full$UnitofTime=="year",365, NA))))
    
    full$AgeinDays = full$TimeValue * full$multiplier
    summary(full$AgeinDays)
    
    full$Name = ifelse(full$Name=="", "Nameless", full$Name)
    sum(ifelse(full$Name=="Nameless",1,0))
    
    full$HasName[full$Name=="Nameless"] = 0
    full$HasName[full$Name!="Nameless"] = 1
    
    table(full$SexuponOutcome)
    
    full$SexuponOutcome = ifelse(nchar(full$SexuponOutcome)==0,
                                 "Spayed Female", full$SexuponOutcome)
    
    full$Hour = hour(full$DateTime)
    full$Weekday = wday(full$DateTime)
    full$Month = month(full$DateTime)
    full$Year = year(full$DateTime)
    
    full$TimeofDay = ifelse(full$Hour>5 & full$Hour<11, "morning",
                            ifelse(full$Hour>10 & full$Hour<16, "midday",
                                   ifelse(full$Hour>15 & full$Hour<20, "lateday", "night")))
    
    table(full$OutcomeType[full$TimeofDay=="midday"])
    
    full$TimeofDay = factor(full$TimeofDay,
                            levels = c("morning", "midday",
                                       "lateday", "night"))
    
    daytimes = full[1:26729, ]
    daytimes = group_by(daytimes, AnimalType, TimeofDay, OutcomeType)
    daytimes = summarise(daytimes, num_animals=n())
    
    output$wondoo2 <- renderPlot({
      #시각화
      ggplot(daytimes, aes(x=TimeofDay, y=num_animals, fill=OutcomeType)) +
       geom_bar(stat="identity", position = "fill", color = "black") +
       facet_wrap(~AnimalType) +
       coord_flip() +
       labs(y = "Proportion of Animals",
             x = "Animal",
             title = "Outcomes by Time of Day : Cats & Dogs") +
       theme_few()
    })
    
    
    table(full$breed)
    temp = factor(full$Breed)
    nrow(table(temp))
    
    levels(factor(full$Breed))[1:10]
    
    full$IsMix = ifelse(grepl("Mix", full$Breed), 1, 0)
    sum(full$IsMix)
    
    temp = strsplit(full$Breed, split="/")
    full$SimpleBreed = sapply(full$Breed, function(x){
      gsub("Mix", "", strsplit(x, split="/")[[1]][2])
    })
    
    head(full$SimpleBreed, 10)
    nrow(table(full$SimpleBreed))
    
    full$Color
    full$SimpleColor = sapply(full$Color, function(x){
      strsplit(x, split="/")[[1]][1]
    })
    
    full$SimpleColor
    nrow(table(full$SimpleColor))
    levels(factor(full$SimpleColor))
    
    table(full$SexuponOutcome)
    
    full$Intact = ifelse(grepl("Intact", full$SexuponOutcome), 1, 
                         ifelse(grepl("Unknown", full$SexuponOutcome), "Unknown", 0))
    full$Sex    = ifelse(grepl("Male", full$SexuponOutcome), "Male",
                         ifelse(grepl("Unknown", full$SexuponOutcome), "Unknown", "Female")) 
    table(full$Intact)
    table(full$Sex)
    
    Intact = full[1:26729, ]
    Intact = group_by(Intact, AnimalType, Intact, OutcomeType)
    Intact = summarise(Intact, num_animals=n())
    
    
    output$wondoo3 <- renderPlot({
    
      #시각화
      ggplot(Intact, aes(x=Intact, y=num_animals, fill=OutcomeType)) +
        geom_bar(stat="identity", position = "fill", color="black") +
        facet_wrap(~AnimalType) +
        coord_flip() +
        labs(y="Proportion of Animals",
             x="Animal",
             title="Outcomes by Intactness: Cats & Dogs") +
        theme_few()
    
    })
    
    
    table(full$AgeuponOutcome)
    sum(is.na(full$AgeinDays))
    sum(is.na(full$AgeuponOutcome))
    
    age_fit = rpart(AgeinDays ~ AnimalType + Sex + Intact + SimpleBreed + HasName,
                    data = full[!is.na(full$AgeinDays), ],
                    method = "anova")
    
    str(age_fit)
    class(age_fit)
    
    full$AgeinDays[is.na(full$AgeinDays)] = predict(age_fit, full[is.na(full$AgeinDays), ])
    summary(full$AgeinDays)
    sum(is.na(full$AgeinDays))
    
    full$Lifestage[full$AgeinDays < 365] = "baby"
    full$Lifestage[full$AgeinDays >= 365] = "adult"
    str(full$Lifestage)
    full$Lifestage = factor(full$Lifestage)
    class(full$Lifestage)
    
    
    output$wondoo4 <- renderPlot({
    
    #시각화
    ggplot(full[1:26729, ], aes(x=Lifestage, fill=OutcomeType)) + 
      geom_bar(position = "fill", color="black") + 
      labs(y = "Proportion", title="Animal Outcome : Babies versus Adults") + 
      theme_few()
   
    })  
       
    factorVars = c("Name","OutcomeType", "OutcomeSubtype", "AnimalType",
                   "SexuponOutcome", "AgeuponOutcome", "SimpleBreed", "SimpleColor",
                   "HasName", "IsMix", "Intact", "Sex", "TimeofDay", "Lifestage")
    
    full = data.frame(full)
    full[factorVars] = lapply(full[factorVars], function(x){
      as.factor(x)
    })
    
    train = full[1:26729, ]
    test = full[26730:nrow(full), ]
    set.seed(731)
    
    nrow(table(full$SimpleColor))
    
    rf_mod = randomForest(OutcomeType ~ AnimalType+AgeinDays+Intact+HasName+Hour+Weekday+TimeofDay+IsMix+Sex+Month, 
                          data = train, 
                          ntree = 600, 
                          importance = TRUE)
    
    importance = importance(rf_mod)
    varImportance = data.frame(Variables = row.names(importance),
                               Importance = round(importance[,"MeanDecreaseGini"],2))
    
    output$wondoo5 <- renderPlot({
    
    #시각화
    ggplot(varImportance, aes(x=reorder(Variables, Importance),
                              y=Importance)) +
      geom_bar(stat="identity", colour="black") +
      labs(x="Variables", title="Relative Variable Importance") +
      coord_flip() +
      theme_few()
    
    })
      
    prediction = predict(rf_mod, test, type="response")
    
    
    output$wondoo6 <- renderTable({
    
    #시각화
    solution = data.table("ID"=test$ID, prediction)
    table(solution)
    
    })
    
  }

  
}#shiny app End
