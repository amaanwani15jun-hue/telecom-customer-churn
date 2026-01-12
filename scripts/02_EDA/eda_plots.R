choices <- unique(cleaned_data$internet_service)
cat("What is your choice\n")
i <- 1

while(i <= 3) {
  cat(i , "." , choices[i] , "\n" )
  i <-  i +1
}
choice_taken <- as.integer(readline("You Choose : "))

output <- cleaned_data |> 
  filter(internet_service== choices[choice_taken]) |> 
  group_by(gender) |>
  summarise(Total = n()) |> 
  ungroup()

print(output)





