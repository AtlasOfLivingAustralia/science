add_author_profile_photo = function(img_src, github_url){
  tags$div(
    id = "pictureposition",
    tags$img(class="clipped",
             src = img_src,
             style = "height:120px"),
    tags$div(
      id = "linkposition",
      tags$button(
        class = "author-button",
        tags$i(
          href = github_url, # personal website
          class = "fab fa-github"
        )
      )
    )
  )
}
