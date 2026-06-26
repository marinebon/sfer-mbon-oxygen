# 1. copy example_batch folder
# 2. rename folder to your batch_name
# 3. modify generate_batch_qmds.R
#   1. modify BATCH_NAME to match your batch_name
#   2. modify EXAMPLE_BATCH_VALUE to match an example value from your data
# 4. modify template.qmd params
#   1. use same EXAMPLE_BATCH_VALUE from (3) for batch_value
#   2. modify batch_name to match your batch_name
# 5. modify _quarto.yml
#   1. add `{batch_name}/generate_batch_qmds` to pre-render
#   2. add `{batch_name}/batched_reports/*.qmd` to render
#   3. add `{batch_name}/listing.qmd` to render
#   4. add `{batch_name}/listing.qmd` to navbar


create_batch <- function(batch_name, example_batch_value) {
  # 1. copy example_batch folder
  if (!dir.exists("example_batch")) {
    stop("example_batch folder does not exist")
  }
  
  if (dir.exists(batch_name)) {
    stop(paste("Directory", batch_name, "already exists"))
  }
  
  # Copy the example_batch folder
  system(paste("cp -r example_batch", batch_name))
  
  # 2. modify generate_batch_qmds.R
  generate_file <- file.path(batch_name, "generate_batch_qmds.R")
  if (file.exists(generate_file)) {
    content <- readLines(generate_file)
    
    # Replace BATCH_NAME
    content <- gsub('BATCH_NAME <- "example_batch"', 
                    paste0('BATCH_NAME <- "', batch_name, '"'), 
                    content)
    
    # Replace EXAMPLE_BATCH_VALUE
    content <- gsub('"example_1"', 
                    paste0('"', example_batch_value, '"'), 
                    content)
    
    writeLines(content, generate_file)
  }
  
  # 5. modify template.qmd params
  template_file <- file.path(batch_name, "template.qmd")
  if (file.exists(template_file)) {
    content <- readLines(template_file)
    
    # Replace batch_value in params
    content <- gsub('batch_value: "example_1"', 
                    paste0('batch_value: "', example_batch_value, '"'), 
                    content)
    
    # Replace batch_name in params
    content <- gsub('batch_name: "example_batch"', 
                    paste0('batch_name: "', batch_name, '"'), 
                    content)
    
    # Replace title
    content <- gsub('title: "example_1 Report"', 
                    paste0('title: "', example_batch_value, ' Report"'), 
                    content)
    
    writeLines(content, template_file)
  }
  
  # 6. modify _quarto.yml using yaml library
  quarto_file <- "_quarto.yml"
  if (file.exists(quarto_file)) {
    if (!nzchar(system.file(package = "yaml"))) {
      install.packages("yaml")
    }
    library(yaml)
    
    # Read and parse the YAML
    yaml_content <- yaml::read_yaml(quarto_file)
    
    # Add to pre-render section
    if (is.null(yaml_content$project$`pre-render`)) {
      yaml_content$project$`pre-render` <- paste0(batch_name, "/generate_batch_qmds.R")
    } else {
      yaml_content$project$`pre-render` <- c(
        yaml_content$project$`pre-render`,
        paste0(batch_name, "/generate_batch_qmds.R")
      )
    }
    
    # Add to render section
    if (is.null(yaml_content$project$render)) {
      yaml_content$project$render <- c(
        paste0(batch_name, "/batched_reports/*.qmd"),
        paste0(batch_name, "/listing.qmd")
      )
    } else {
      yaml_content$project$render <- c(
        yaml_content$project$render,
        paste0(batch_name, "/batched_reports/*.qmd"),
        paste0(batch_name, "/listing.qmd")
      )
    }
    
    # Add to navbar
    if (is.null(yaml_content$website$navbar$left)) {
      yaml_content$website$navbar$left <- list(
        paste0(batch_name, "/listing.qmd")
      )
    } else {
      # Find the GitHub link and insert before it
      navbar_items <- yaml_content$website$navbar$left
      github_index <- which(sapply(navbar_items, function(x) {
        if (is.list(x) && !is.null(x$href)) {
          x$href == "https://github.com/7yl4r/quartobatch"
        } else if (is.character(x)) {
          x == "https://github.com/7yl4r/quartobatch"
        } else {
          FALSE
        }
      }))
      
      new_nav_item <- paste0(batch_name, "/listing.qmd")
      
      if (length(github_index) > 0) {
        # Insert before GitHub link
        yaml_content$website$navbar$left <- append(navbar_items, list(new_nav_item), after = github_index - 1)
      } else {
        # GitHub link not found, append to end
        yaml_content$website$navbar$left <- c(navbar_items, list(new_nav_item))
      }
    }
    
    # Write back the YAML
    writeLines(as.yaml(yaml_content), quarto_file)
    
    # Fix boolean values from yes/no back to true/false
    content <- readLines(quarto_file)
    content <- gsub("toc: yes", "toc: true", content)
    content <- gsub("code-fold: yes", "code-fold: true", content)
    content <- gsub("message: no", "message: false", content)
    content <- gsub("warning: no", "warning: false", content)
    writeLines(content, quarto_file)
  }

  # 7. modify the listing to replace "Example Reports" with the batch name
  listing_file <- file.path(batch_name, "listing.qmd")
  if (file.exists(listing_file)) {
    content <- readLines(listing_file)
    content <- gsub("Example Reports", batch_name, content)
    writeLines(content, listing_file)
  }
  
  print(paste("Successfully created batch:", batch_name))
  print(paste("Example batch value:", example_batch_value))
  print("Don't forget to:")
  print("1. Modify getData.R and getListOfValues.R for your specific data")
  print("2. Add the listing.qmd to the navbar in _quarto.yml if needed")
}
