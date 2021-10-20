library(shiny)
library("glue")
library(dplyr)
library(ggplot2)
library(gridExtra)

####################################
# set directory and files          #
####################################
# files 
dir_name_1="Inputs/"

####################################
# Merics functions                #
####################################

metric_cal =function (s_dmd, s_pload, s_ptrain, s_conn, s_energy, group, lv){ 
  B_by_op_seg = read.csv(glue('{dir_name_1}base_by_op_seg.csv'), stringsAsFactors = F)
  B_system = read.csv(glue('{dir_name_1}base_system.csv'), stringsAsFactors = F)
  line_types <- c("Base"="dashed","Scenario"="solid")
  if (s_dmd == "BAU-freightProj" &&
      s_pload == "BAU-payload" && 
      s_ptrain =="AEO21-powertrainAdopt"&& 
      s_conn ==  "BAU-connectivity" &&
      s_energy == "Base-wtwEmissions") {
    if (group == "segment"){
      result_df= subset(B_by_op_seg, analysis_year <=2050)
      if (lv =="total"){
        g_energy=ggplot(data=result_df)+
          geom_line(aes(x=analysis_year, y=Base_Mbtu, group=op_segment, color=op_segment), linetype=line_types[['Base']])+
          xlab("Year")+
          ylab("Million BTU")+
          scale_color_manual(name ="",values = c("First-mile"="darkred","Last-mile"="blue", "Local-Regional"="green", "Long-haul"="purple"))+
          ggtitle("Total Energy")
        g_cost=ggplot(data=result_df)+
          geom_line(aes(x=analysis_year, y=Base_Mdollar, group=op_segment, color=op_segment), linetype=line_types[['Base']])+
          xlab("Year")+
          ylab("Million $")+
          scale_color_manual(name ="",values = c("First-mile"="darkred","Last-mile"="blue", "Local-Regional"="green", "Long-haul"="purple"))+
          ggtitle("Total Operational Cost")
        g_emission=ggplot(data=result_df)+
          geom_line(aes(x=analysis_year, y=Base_Mco2kg, group=op_segment, color=op_segment), linetype=line_types[['Base']])+
          xlab("Year")+
          ylab("Million Co2_kg")+
          scale_color_manual(name ="",values = c("First-mile"="darkred","Last-mile"="blue", "Local-Regional"="green", "Long-haul"="purple"))+
          ggtitle("Total Well to Wheel Co2 Emission")
      } else {
        g_energy=ggplot(data=result_df)+
          geom_line(aes(x=analysis_year, y=Base_btu_per_mi, group=op_segment, color=op_segment), linetype=line_types[['Base']])+
          xlab("Year")+
          ylab("BTU/miles")+
          scale_color_manual(name ="",values = c("First-mile"="darkred","Last-mile"="blue", "Local-Regional"="green", "Long-haul"="purple"))+
          ggtitle("Levelized Energy")
        g_cost=ggplot(data=result_df)+
          geom_line(aes(x=analysis_year, y=Base_dollar_per_mi, group=op_segment, color=op_segment), linetype=line_types[['Base']])+
          xlab("Year")+
          ylab("$/miles")+
          scale_color_manual(name ="",values = c("First-mile"="darkred","Last-mile"="blue", "Local-Regional"="green", "Long-haul"="purple"))+
          ggtitle("Levelized Operational Cost")
        g_emission=ggplot(data=result_df)+
          geom_line(aes(x=analysis_year, y=Base_co2kg_per_mi, group=op_segment, color=op_segment), linetype=line_types[['Base']])+
          xlab("Year")+
          ylab("Co2_kg/miles")+
          scale_color_manual(name ="",values = c("First-mile"="darkred","Last-mile"="blue", "Local-Regional"="green", "Long-haul"="purple"))+
          ggtitle("Levelized Well to Wheel Co2 Emission")
      }
    } else {
      result_df = subset(B_system,analysis_year <=2050)
      if (lv =="total"){
        g_energy=ggplot(data=result_df)+
          geom_line(aes(x=analysis_year, y=Base_Mbtu), linetype=line_types[['Base']])+
          xlab("Year")+
          ylab("Million BTU")+
          ggtitle("Total Energy")
        g_cost=ggplot(data=result_df)+
          geom_line(aes(x=analysis_year, y=Base_Mdollar), linetype=line_types[['Base']])+
          xlab("Year")+
          ylab("Million $")+
          ggtitle("Total Operational Cost")
        g_emission=ggplot(data=result_df)+
          geom_line(aes(x=analysis_year, y=Base_Mco2kg), linetype=line_types[['Base']])+
          xlab("Year")+
          ylab("Million Co2_kg")+
          ggtitle("Total Well to Wheel Co2 Emission")
      } else {
        g_energy=ggplot(data=result_df)+
          geom_line(aes(x=analysis_year, y=Base_btu_per_mi), linetype=line_types[['Base']])+
          xlab("Year")+
          ylab("BTU/miles")+
          ggtitle("Levelized Energy")
        g_cost=ggplot(data=result_df)+
          geom_line(aes(x=analysis_year, y=Base_dollar_per_mi), linetype=line_types[['Base']])+
          xlab("Year")+
          ylab("$/miles")+
          ggtitle("Levelized Operational Cost")
        g_emission=ggplot(data=result_df)+
          geom_line(aes(x=analysis_year, y=Base_co2kg_per_mi), linetype=line_types[['Base']])+
          xlab("Year")+
          ylab("Co2_kg/miles")+
          ggtitle("Levelized Well to Wheel Co2 Emission")
      }
    }
  } else {
    input_df= read.csv(glue('{dir_name_1}{s_dmd}_{s_pload}_{s_ptrain}_{s_conn}_{s_energy}.csv'), stringsAsFactors = F)
    if (group == "segment"){
      S_by_op_seg = input_df %>% group_by(op_segment, analysis_year) %>%  summarise(Scenario_btu_per_mi= sum(energy_btu)/sum(vmt), 
                                                                                    Scenario_co2kg_per_mi= sum(co2e_mtonnes)/sum(vmt)*10^3,  
                                                                                    Scenario_dollar_per_mi= sum(discounted_cost)/sum(discounted_vmt),
                                                                                    Scenario_Mbtu= sum(energy_btu)/10^6,
                                                                                    Scenario_Mco2kg= sum(co2e_mtonnes)/10^6,
                                                                                    Scenario_Mdollar= sum(discounted_cost)/10^6, .groups = 'drop')
      result_df= merge(B_by_op_seg,S_by_op_seg, by = c('op_segment', 'analysis_year'))
      result_df= subset(result_df, analysis_year<=2050)
      #result_df= subset(result_df, select =-c(X))
      if (lv =="total"){
        g_energy=ggplot(data=result_df)+
          geom_line(aes(x=analysis_year, y=Base_Mbtu, group=op_segment, color=op_segment), linetype=line_types[['Base']])+
          geom_line(aes(x=analysis_year, y=Scenario_Mbtu, group=op_segment, color=op_segment), linetype=line_types[["Scenario"]])+
          xlab("Year")+
          ylab("Millon BTU")+
          scale_color_manual(name ="",values = c("First-mile"="darkred","Last-mile"="blue", "Local-Regional"="green", "Long-haul"="purple"))+
          ggtitle("Total Energy: baseline(dashed), scenario(solid)")+
          scale_linetype_manual(name="", values=c("base"="dashed", "scenario"="solid"))
        print (g_energy)
        g_cost=ggplot(data=result_df)+
          geom_line(aes(x=analysis_year, y=Base_Mdollar, group=op_segment, color=op_segment), linetype=line_types[['Base']])+
          geom_line(aes(x=analysis_year, y=Scenario_Mdollar, group=op_segment, color=op_segment), linetype=line_types[["Scenario"]])+
          xlab("Year")+
          ylab("Millon $")+
          scale_color_manual(name ="",values = c("First-mile"="darkred","Last-mile"="blue", "Local-Regional"="green", "Long-haul"="purple"))+
          ggtitle("Total Operational Cost: baseline(dashed), scenario(solid)")+
          scale_linetype_manual(values=line_types)
        g_emission=ggplot(data=result_df)+
          geom_line(aes(x=analysis_year, y=Base_Mco2kg, group=op_segment, color=op_segment), linetype=line_types[['Base']])+
          geom_line(aes(x=analysis_year, y=Scenario_Mco2kg, group=op_segment, color=op_segment), linetype=line_types[["Scenario"]])+
          xlab("Year")+
          ylab("Millon Co2_kg")+
          scale_color_manual(name ="",values = c("First-mile"="darkred","Last-mile"="blue", "Local-Regional"="green", "Long-haul"="purple"))+
          ggtitle("Total Well to Wheel Co2 Emission: baseline(dashed), scenario(solid)")+
          scale_linetype_manual(values=line_types)
      } else{
        g_energy=ggplot(data=result_df)+
          geom_line(aes(x=analysis_year, y=Base_btu_per_mi, group=op_segment, color=op_segment), linetype=line_types[['Base']])+
          geom_line(aes(x=analysis_year, y=Scenario_btu_per_mi, group=op_segment, color=op_segment), linetype=line_types[["Scenario"]])+
          xlab("Year")+
          ylab("BTU/miles")+
          scale_color_manual(name ="",values = c("First-mile"="darkred","Last-mile"="blue", "Local-Regional"="green", "Long-haul"="purple"))+
          ggtitle("Levelized Energy: baseline(dashed), scenario(solid)")+
          scale_linetype_manual(name="", values=c("base"="dashed", "scenario"="solid"))
        print (g_energy)
        g_cost=ggplot(data=result_df)+
          geom_line(aes(x=analysis_year, y=Base_dollar_per_mi, group=op_segment, color=op_segment), linetype=line_types[['Base']])+
          geom_line(aes(x=analysis_year, y=Scenario_dollar_per_mi, group=op_segment, color=op_segment), linetype=line_types[["Scenario"]])+
          xlab("Year")+
          ylab("$/miles")+
          scale_color_manual(name ="",values = c("First-mile"="darkred","Last-mile"="blue", "Local-Regional"="green", "Long-haul"="purple"))+
          ggtitle("Levelized Operational Cost: baseline(dashed), scenario(solid)")+
          scale_linetype_manual(values=line_types)
        g_emission=ggplot(data=result_df)+
          geom_line(aes(x=analysis_year, y=Base_co2kg_per_mi, group=op_segment, color=op_segment), linetype=line_types[['Base']])+
          geom_line(aes(x=analysis_year, y=Scenario_co2kg_per_mi, group=op_segment, color=op_segment), linetype=line_types[["Scenario"]])+
          xlab("Year")+
          ylab("Co2_kg/miles")+
          scale_color_manual(name ="",values = c("First-mile"="darkred","Last-mile"="blue", "Local-Regional"="green", "Long-haul"="purple"))+
          ggtitle("Levelized Well to Wheel Co2 Emission: baseline(dashed), scenario(solid)")+
          scale_linetype_manual(values=line_types)
      }
      
      
    } else {
      S_system = input_df %>% group_by(analysis_year) %>%  summarise(Scenario_btu_per_mi= sum(energy_btu)/sum(vmt), 
                                                                     Scenario_co2kg_per_mi= sum(co2e_mtonnes)/sum(vmt)*10^3,  
                                                                     Scenario_dollar_per_mi= sum(discounted_cost)/sum(discounted_vmt),
                                                                     Scenario_Mbtu= sum(energy_btu)/10^6,
                                                                     Scenario_Mco2kg= sum(co2e_mtonnes)/10^6,
                                                                     Scenario_Mdollar= sum(discounted_cost)/10^6, .groups = 'drop')
      result_df =merge(B_system,S_system, by = 'analysis_year')
      result_df= subset(result_df, analysis_year<=2050)
      #result_df= subset(result_df, select =-c(X))
      if (lv =="total"){
        g_energy=ggplot(data=result_df)+
          geom_line(aes(x=analysis_year, y=Base_Mbtu), linetype=line_types[['Base']])+
          geom_line(aes(x=analysis_year, y=Scenario_Mbtu), linetype=line_types[["Scenario"]])+
          xlab("Year")+
          ylab("Million BTU")+
          ggtitle("Total Energy: baseline(dashed), scenario(solid)")+
          scale_linetype_manual(values=line_types)
        g_cost=ggplot(data=result_df)+
          geom_line(aes(x=analysis_year, y=Base_Mdollar), linetype=line_types[['Base']])+
          geom_line(aes(x=analysis_year, y=Scenario_Mdollar), linetype=line_types[["Scenario"]])+
          xlab("Year")+
          ylab("Million $")+
          ggtitle("Total Operational Cost: baseline(dashed), scenario(solid)")+
          scale_linetype_manual(values=line_types)
        g_emission=ggplot(data=result_df)+
          geom_line(aes(x=analysis_year, y=Base_Mco2kg), linetype=line_types[['Base']])+
          geom_line(aes(x=analysis_year, y=Scenario_Mco2kg), linetype=line_types[["Scenario"]])+
          xlab("Year")+
          ylab("Million Co2_kg")+
          ggtitle("Total Well to Wheel Co2 Emission: baseline(dashed), scenario(solid)")+
          scale_linetype_manual(values=line_types)
      }
      else {
        g_energy=ggplot(data=result_df)+
          geom_line(aes(x=analysis_year, y=Base_btu_per_mi), linetype=line_types[['Base']])+
          geom_line(aes(x=analysis_year, y=Scenario_btu_per_mi), linetype=line_types[["Scenario"]])+
          xlab("Year")+
          ylab("BTU/miles")+
          ggtitle("Levelized Energy: baseline(dashed), scenario(solid)")+
          scale_linetype_manual(values=line_types)
        g_cost=ggplot(data=result_df)+
          geom_line(aes(x=analysis_year, y=Base_dollar_per_mi), linetype=line_types[['Base']])+
          geom_line(aes(x=analysis_year, y=Scenario_dollar_per_mi), linetype=line_types[["Scenario"]])+
          xlab("Year")+
          ylab("$/miles")+
          ggtitle("Levelized Operational Cost: baseline(dashed), scenario(solid)")+
          scale_linetype_manual(values=line_types)
        g_emission=ggplot(data=result_df)+
          geom_line(aes(x=analysis_year, y=Base_co2kg_per_mi), linetype=line_types[['Base']])+
          geom_line(aes(x=analysis_year, y=Scenario_co2kg_per_mi), linetype=line_types[["Scenario"]])+
          xlab("Year")+
          ylab("Co2_kg/miles")+
          ggtitle("Levelized Well to Wheel Co2 Emission: baseline(dashed), scenario(solid)")+
          scale_linetype_manual(values=line_types)
      }
    }
  }
  return (list(result_df, g_energy, g_cost, g_emission))
}

function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    rst <- metric_cal(input$s_dmd, input$s_pload, input$s_ptrain, input$s_conn, input$s_energy, input$group, input$lv)
    rst
  })
  
  output$testHTML <- renderText({
    if (input$submitbutton>0) {
      paste("<b>Status: Metrics are calculated at the", input$group, "level", "<br>", 
            "You have selected: </br>","<ul>",
            "<li>", input$s_dmd,  "</li>", "<li>", input$s_pload,  "</li>",
            "<li>", input$s_ptrain,  "</li>", "<li>", input$s_conn,  "</li>", "<li>", input$s_energy, "</li>", "</ul>")
    } else {
      paste("<b>Status: Server is ready for calculation.")
    }
  })
  
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      glue('{input$s_dmd}_{input$s_pload}_{input$s_ptrain}_{input$s_conn}_{input$s_energy}.csv')
    },
    content = function(file) {
      rst <- datasetInput()
      write.csv(rst[[1]], file, row.names = FALSE)
    }
  )
  
  output$table <- renderTable({
    if (input$submitbutton>0) {
      rst <- datasetInput()
      isolate(rst[[1]])
    }
  })
  
  output$plot1 = renderPlot({
    if (input$submitbutton>0) {
      rst <- datasetInput()
      print (rst[[2]])
    }})
  
  output$plot2 = renderPlot({
    if (input$submitbutton>0) {
      rst <- datasetInput()
      print (rst[[3]])
    }})
  
  output$plot3 = renderPlot({
    if (input$submitbutton>0) {
      rst <- datasetInput()
      print (rst[[4]])
    }})
  
}
  


