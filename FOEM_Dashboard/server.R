library(shiny)

####################################
# set directory and files          #
####################################
# files 
dir_name="Input_files/"
a_file_name = "Activity_mock_up.csv"
p_file_name = "Performance_mock_up.csv"
i_file_name = "Impact_mock_up.csv"

# set segment list
segment_list = c('LH','RH') # c('FM',LH','RH', 'LM')

# input tables by reading files 
A_table= read.csv(paste0(dir_name, a_file_name), stringsAsFactors = F)
A_table$mean_speed = A_table$mean_dist/A_table$mean_OR_time*60
P_table= read.csv(paste0(dir_name, p_file_name), stringsAsFactors = F)
I_table= read.csv(paste0(dir_name, i_file_name), stringsAsFactors = F)


####################################
# Merics functions                #
####################################

## Unit energy_consumption function 
unit_energy_consumption =function (A,P,I,tech,t_lv,t_delta, I_var){ 
  # Definition of fuction variables:  
  # A: activity table, P: performance tabel, 
  # I: impact change table, Tech: tech x, I_var: where technology x have impact
  ## set output file
  output_df = data.frame(segment = character(), 
                         metric = integer())
  ## calcuation based on the example slides 
  for(s in segment_list) {
    Sub_A = subset(A, segment == s)
    var_1 =0 # energy consumption
    var_2 =0 
    for (i in 1:nrow(Sub_A)) {
      var_1 =  var_1 + Sub_A$mean_dist[i]*Sub_A$n_trips[i]*P[which(P$segment == s & 
                                                                     P$weight.class == Sub_A$weight.class[i] & 
                                                                     P$vehicle.type==Sub_A$vehicle.type[i] & 
                                                                     P$powertrain.type == Sub_A$powertrain.type[i] &
                                                                     P$classfier_1 == Sub_A$classfier_1[i] &
                                                                     P$classfier_2 == Sub_A$classfier_2[i] &
                                                                     P$operation_type == "road" &
                                                                     (P$spd_bin_min <= Sub_A$mean_speed[i] & P$spd_bin_max > Sub_A$mean_speed[i])), 'fuel_rate'] *
        (1+I[which(I$tech== tech & I$impact_on== I_var & I$tech_level==t_lv), 'derivative']*t_delta/100)+
        Sub_A$mean_idling_time[i]*Sub_A$n_idling[i]*P[which(P$segment == s & 
                                                              P$weight.class == Sub_A$weight.class[i] & 
                                                              P$vehicle.type==Sub_A$vehicle.type[i] & 
                                                              P$powertrain.type == Sub_A$powertrain.type[i] &
                                                              P$classfier_1 == Sub_A$classfier_1[i] &
                                                              P$classfier_2 == Sub_A$classfier_2[i] &
                                                              P$operation_type == "idling"), 'fuel_rate']*
        (1+I[which(I$tech== tech & I$impact_on== I_var & I$tech_level==t_lv), 'derivative']*t_delta/100)
      
      var_2 = var_2 + Sub_A$mean_dist[i]*Sub_A$n_trips[i]
    }
    
    output_df = rbind(output_df, data.frame(segment = s, metric =var_1/var_2))
  }
  return (output_df)
}

## Unit energy_consumption function 
## note: for the demo version, I put var=5 as an initial setting. Need to update for real 
unit_cost =function (A,P,I,tech,t_lv,t_delta, I_var){ 
  # Definition of fuction variables:  
  # A: activity table, P: performance tabel, 
  # I: impact change table, Tech: tech x, I_var: where technology x have impact
  ## set output file
  output_df = data.frame(segment = character(), 
                         metric = integer())
  ## calcuation based on the example slides 
  for(s in segment_list) {
    Sub_A = subset(A, segment == s)
    var_1 =5 # energy consumption  
    var_2 =0  
    for (i in 1:nrow(Sub_A)) {
      var_1 =  var_1 + Sub_A$mean_dist[i]*Sub_A$n_trips[i]*P[which(P$segment == s & 
                                                                     P$weight.class == Sub_A$weight.class[i] & 
                                                                     P$vehicle.type==Sub_A$vehicle.type[i] & 
                                                                     P$powertrain.type == Sub_A$powertrain.type[i] &
                                                                     P$classfier_1 == Sub_A$classfier_1[i] &
                                                                     P$classfier_2 == Sub_A$classfier_2[i] &
                                                                     P$operation_type == "road" &
                                                                     (P$spd_bin_min <= Sub_A$mean_speed[i] & P$spd_bin_max > Sub_A$mean_speed[i])), 'fuel_rate'] *
        (1+I[which(I$tech== tech & I$impact_on== I_var & I$tech_level==t_lv), 'derivative']*t_delta/100)+
        Sub_A$mean_idling_time[i]*Sub_A$n_idling[i]*P[which(P$segment == s & 
                                                              P$weight.class == Sub_A$weight.class[i] & 
                                                              P$vehicle.type==Sub_A$vehicle.type[i] & 
                                                              P$powertrain.type == Sub_A$powertrain.type[i] &
                                                              P$classfier_1 == Sub_A$classfier_1[i] &
                                                              P$classfier_2 == Sub_A$classfier_2[i] &
                                                              P$operation_type == "idling"), 'fuel_rate']*
        (1+I[which(I$tech== tech & I$impact_on== I_var & I$tech_level==t_lv), 'derivative']*t_delta/100)
      
      var_2 = var_2 + Sub_A$mean_dist[i]*Sub_A$n_trips[i]
    }
    
    output_df = rbind(output_df, data.frame(segment = s, metric =var_1/var_2))
  }
  return (output_df)
}

####################################
# Merics functions base vs scenario #
####################################

## Unit energy_consumption function 
S_unit_energy_consumption =function (A,P,I,tech,t_lv,t_delta, I_var){ 
  if ( t_lv == 0 & t_delta ==0) {
    output= unit_energy_consumption(A,P,I,tech,t_lv,t_delta, I_var)
    names(output)[names(output) == 'metric'] <- 'baseline metric'
  } else{
    output= unit_energy_consumption(A,P,I,tech,0,0, I_var)
    names(output)[names(output) == 'metric'] <- 'baseline metric'
    output2= unit_energy_consumption(A,P,I,tech,t_lv,t_delta, I_var)
    names(output2)[names(output2) == 'metric'] <- paste0('Scenario metric','(Level:',t_lv, '|Change:',t_delta, '%)')
    output=merge(output, output2, by='segment')
  }
  return (output)
}

## Unit energy_consumption function 
S_unit_cost =function (A,P,I,tech,t_lv,t_delta, I_var){ 
  if ( t_lv == 0 & t_delta ==0) {
    output= unit_energy_consumption(A,P,I,tech,t_lv,t_delta, I_var)
    names(output)[names(output) == 'metric'] <- 'baseline metric'
  } else{
    output= unit_energy_consumption(A,P,I,tech,0,0, I_var)
    names(output)[names(output) == 'metric'] <- 'baseline metric'
    output2= unit_energy_consumption(A,P,I,tech,t_lv,t_delta, I_var)
    names(output2)[names(output2) == 'metric'] <- paste0('Scenario metric','(Level:',t_lv, '|Change:',t_delta, '%)')
    output=merge(output, output2, by='segment')
  }
  return (output)
}

function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    if (input$metric == "Unit Energy Consumption"){
     S_unit_energy_consumption(A_table, P_table, I_table, input$tech, input$t_lv,input$t_delta, 'fuel rate')
    } else if (input$metric == "Unit Cost"){
     S_unit_cost(A_table, P_table, I_table, input$tech, input$t_lv,input$t_delta, 'fuel rate')
    } 
    #return(Output)
  })
  
  # Status/Output Text Box
  # output$contents <- renderPrint({
  #   if (input$submitbutton>0) { 
  #     isolate("Calculation complete.") 
  #   } else {
  #     return("Server is ready for calculation.")
  #   }
  # })
  output$contents <-renderText({
    if (input$submitbutton>0) {
      paste("Status:", as.character(input$metric) ,"calculation complete and server is ready for another calucation.")
    } else {
      paste("Status: Server is ready for calculation.")
    }
  })
  
  output$button <- renderUI({
    if (input$submitbutton>0){
      downloadButton("downloadData", label ="Download")
    }})
  

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$metric, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(datasetInput(), file, row.names = FALSE)
      }
  )
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(print(datasetInput())) 
    } 
  })
  }
  


