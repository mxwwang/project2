cstat_to_colnames <- function(covid_stat) {
  
  if (covid_stat == "Vaccinations") {
    colname = "vx_fully_perc"
  }  else if (covid_stat == "Hospitalizations") {
    colname = "hosp_count"
  } else if (covid_stat == "Deaths") {
    colname = "death_count"
  } else {
    colname = "case_count"
  }
  
  return(colname)
}

### function 1 - mobility across boroughs of 1 category
plots_by_category <- function(category){
  
  category = gsub(" ","_",tolower(category))
  
  plt_dot = c("date","borough",category)
  plt_bar = c("date","borough",category)
  
  title_dot = gsub("And", "&", paste(paste(str_to_title(strsplit(category, "_")[[1]]),collapse = " "),"Change from Baseline (%)",sep=" "))
  title_violin = gsub("And", "&", paste("Distribution of",paste(str_to_title(strsplit(category, "_")[[1]]),collapse = " "),"Change",sep=" "))
  title_bar = gsub("And", "&", paste("Median of", paste(str_to_title(strsplit(category, "_")[[1]]),collapse = " "),"Change",sep=" "))
    
  # 1 - dot plot
  p1 = mobility_nyc %>%
    select(one_of(plt_dot)) %>%
    rename(feature = category) %>%
    group_by(borough) %>%
    ggplot() +
    geom_point(aes(x=date,y=feature,color=borough), size=1, alpha=0.4) +
    labs(title = title_dot, x="Date",y="Percentage Change (%)",color="Borough") +
    #ylim(-100,100) +
    geom_hline(yintercept=0, linetype="dashed", color = "red",size = 1)
  
  # 2 - violin
  p2<-  mobility_nyc %>%group_by(borough) %>%
    select(one_of(plt_dot)) %>%
    rename(feature = category) %>%
    ggplot(aes(x = borough, y = feature, fill = borough)) +
    geom_violin(trim = FALSE) +
    geom_boxplot(width = 0.07) + 
    labs(title = title_violin, x="Borough", y="Percentage Change (%)",color="Borough")

  p3 <- mobility_nyc %>%
    select(one_of(plt_bar)) %>%
    rename(feature = category) %>%
    group_by(borough) %>%
    summarise(median = median(feature,na.rm=TRUE))%>%
    ggplot(aes(x=reorder(borough, median,), y=median,fill=borough)) +
    geom_bar(position='dodge', stat='identity')+
    labs(title = title_bar, x="Borough",y="Percentage Change (%)")#,color="Borough")
    #ylim(-80,100)
  
  
  # ggarrange(
  #   p1,                # First row with line plot
  #   # Second row with box and dot plots
  #   ggarrange(p2, p3, ncol = 2)  , #labels = c("B", "C")),
  #   nrow = 3 #,
  #   #labels = "A"       # Label of the line plot
  # )

  layout <- "
  AAA
  AAA
  BBC
  "
  
  p1 + p2 + p3 + plot_layout(design = layout)
  
}


### function 2 - correlation matrix for 1 borough
corr_matrix_plot <- function(region, cutoff_dt){
  
  df_nyc = nyc
  
  if(region != "NYC"){
    
    df_nyc = nyc %>%
      filter(borough==region)
  }
  
  dt_earlest = min(df_nyc$date)
  dt_latest = max(df_nyc$date)
  
  
  if (cutoff_dt < dt_earlest ||  cutoff_dt > dt_latest){
    df_nyc_0 = df_nyc %>%
      #filter(date<= cutoff_dt)%>%
      select(10,11,12,
             13,14,15,
             28,29,30,23,20)
    
    corrplot(cor(df_nyc_0,use="pairwise.complete.obs"), method="color", #col=col(200),
             type="lower", #order="hclust",
             addCoef.col = "black", # Add coefficient of correlation
             tl.col="black", tl.srt=15, #Text label color and rotation
             # Combine with significance
             #p.mat = p.mat, sig.level = 0.01, insig = "blank",
             # hide correlation coefficient on the principal diagonal
             title = "Correlation - Mobility & COVID Stats (Overall)",
             mar=c(0, 0, 1, 0),
             diag=FALSE)
  } else {
    df_nyc_1 = df_nyc %>%
      filter(date<= cutoff_dt)%>%
      select(10,11,12,
             13,14,15,
             28,29,30,23,20)

    df_nyc_2 = df_nyc %>%
      filter(date > cutoff_dt)%>%
      select(10,11,12,
             13,14,15,
             28,29,30,23,20)

    #dim(df_nyc_1)[1] + dim(df_nyc_2)[1] == dim(df_nyc)[1]

    #cor(df_nyc_1,use="pairwise.complete.obs")

    par(mfrow=c(2,1))
    
    c1 = corrplot(cor(df_nyc_1,use="pairwise.complete.obs"), method="color", #col=col(200),
                  type="lower", #order="hclust",
                  addCoef.col = "black", # Add coefficient of correlation
                  tl.col="black", tl.srt=15, #Text label color and rotation
                  # Combine with significance
                  #p.mat = p.mat, sig.level = 0.01, insig = "blank",
                  # hide correlation coefficient on the principal diagonal
                  title = paste("Correlation - Mobility & COVID Stats (Before ", cutoff_dt,")", sep=""),
                  mar=c(0, 0, 1, 0),
                  diag=FALSE
    )

    #cor(df_nyc_2,use="pairwise.complete.obs")
    c2 = corrplot(cor(df_nyc_2,use="pairwise.complete.obs"), method="color", #col=col(200),
                  type="lower", #order="hclust",
                  addCoef.col = "black", # Add coefficient of correlation
                  tl.col="black", tl.srt=15, #Text label color and rotation
                  # Combine with significance
                  #p.mat = p.mat, sig.level = 0.01, insig = "blank",
                  # hide correlation coefficient on the principal diagonal
                  title = paste("Correlation - Mobility & COVID Stats (After ", cutoff_dt,")", sep=""),
                  mar=c(0, 0, 1, 0),
                  diag=FALSE
    )
  }
}
  


### function 3 - correlation matrix for 1 boro
boro_plot <- function(region,covid_stat){
  
  ptitle = paste("Mobility vs. ",covid_stat ,sep="")

  if (covid_stat == "Vaccinations") {
    coeff = 1
    #covid_stat = "vx_fully_perc"
  }  else if (covid_stat == "Hospitalizations") {
    coeff = 6
    #covid_stat = "hosp_count"
  } else if (covid_stat == "Deaths") {
    coeff = 3
    #covid_stat = "death_count"
  } else {
    coeff = 100
    #covid_stat = "case_count"
  }
  
  covid_stat = cstat_to_colnames(covid_stat)
  covid_stat = sym(covid_stat)
  
  # method 2
  case <- 
    nyc %>%
    select(date,borough,(!!covid_stat)) %>%
    filter(borough==region) %>%
    mutate(covid_adj = (!!covid_stat)/coeff) #%>%
  
  
  nyc %>%
    filter(borough==region) %>%
    select(9,10,11,12,13,14,15) %>%
    pivot_longer(cols=-date) %>%
    mutate(name = factor(name, levels = categories_colname,
                         labels = label1 )) %>%
    ggplot(na.rm=T) +
    geom_line(aes(x=date, y=value,color=name),size=1,alpha=0.6)+
    geom_line(data = case,aes(x=date, y=covid_adj),size=1, alpha=0.6)+
    #ylim(-100,100)+
    facet_grid( name ~ .)+
    scale_y_continuous("Mobility change (%)", sec.axis = sec_axis(~.*coeff,name = "Case count")) +
    labs(title = "Mobility change vs Case count", x="Date")+
    guides(color=guide_legend("Category"))
}





  # covid_stat = sym("case_count")
  # nyc %>% 
  #   select(!!covid_stat)
  