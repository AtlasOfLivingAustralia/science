# modals

# group into start page and FAQ modals
# this would also require a 'back' button

help_modal <- function(){
  showModal(
    modalDialog(
      title = "Australia's Biodiversity",
      p("This site shows one 'representative' species for each federal electoral division. It was built by the Science & Decision Support Team at the ", 
      tags$a(href = "https://www.ala.org.au", 
        target = "_blank",
        "Atlas of Living Australia.")),
      h3("Frequently Asked Questions"),
      tags$ul(
        # tags$li(actionLink("help_1", "How do I use this webpage?")), 
        tags$li(actionLink("help_1", "What does 'representative' mean?")),
        tags$li(actionLink("help_2", "What data did you use?")),
        tags$li(actionLink("help_3", "How did you calculate 'representativeness'?"))
      ),
      footer = "Click anywhere to exit",
      easyClose = TRUE
    )
  )
}


# h3("What does 'representative' mean?"),
# p("The ALA contains data sourced from citizen scientists, government departments, and natural history museums and collections. Because of this diversity, the data stored by the ALA tell us a lot about what plants and animals people choose to record in their daily lives. Rather than simply list the most common species in each area, we wanted to identify species that are observed unusually frequently within each area, relative to all other areas in the dataset. WE did this by setting some conditions on which species could be chosen:"),
# tags$ul(
#   tags$li("Only vertebrates were included; no plants or invertebrates"), # add date range too
#   # note above here is all limitations on the data; below are assumptions of the approach. Separate these concepts?
#   tags$li("Species should be relatively common in the dataset; only those species observed in two or more electorates on a total of 10 or more occasions were included"),
#   tags$li("To be chosen, a species should be disproportionately common in an electorate relative to its' abundance in other electorates; this is not the same as choosing the most commonly observed species in each case"),
#   tags$li("Each species should only be represented once; If the same species is detected in more than one electoral division, it is allocated to the region where it has the higher proportional abundance, and the next best-placed species is chosen for the lower-ranked division")
# ),
# h3("How did you calculate representativeness?"),
# p("We  "),



#     paste0(
#       "<h1>Australia Votes!</h1>",
# 
#       "<h3>Biodiversity of Australian Federal Electoral Divisions,
#       2019-2022</h3>",
# 
#       "<p>Data from the <a href='https://ala.org.au/'>Atlas of Living 
#       Australia</a> (ALA) show that Australians have logged 11.2 million
#       observations of plants and animals since 2019; that's almost 70% of the
#       number of votes received in the last federal election.</p>",
# 
#       "<p>We've used that data to find the most representive native vertebrate
#       for each electorate, as determined by the wildlife observers of
#       Australia.</p>"
#     )
