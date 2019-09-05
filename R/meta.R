meta_fields <- function(){
  c(title = "title",
    languageCode = "language",
    createdDate = "created",
    modifiedDate = "modified",
    openFrom = "opened",
    respondentGroup = "respondents",
    editorsContactEmail = "contact",
    codebookActivated = "codebook",
    collectsPersonalData = "personal_data",
    sensitivePersonalDataCollected = "sensitive_data",
    editors = "editors",
    elements = "elements")
}

meta_classes <- function(content){
  nm <- names(content)

  dt <- lapply(nm, meta_change_class, content)
  names(dt) <- nm

  dt
}

meta_change_class <- function(name, content){

  switch(name,
         # characters
         "title" = as.character(content[[name]]),
         "language" = as.character(content[[name]]),
         "respondents" = as.character(content[[name]]),
         "contact"  = as.character(content[[name]]),

         # dates
         "created" = as.Date(content[[name]],
                             origin = "1970-01-01"),
         "modified" = as.Date(content[[name]],
                              origin = "1970-01-01"),
         "opened" = as.Date(content[[name]],
                            origin = "1970-01-01"),

         # logicals
         "codebook" = as.logical(content[[name]]),
         "personal_data" = as.logical(content[[name]]),
         "sensitive_data" = as.logical(content[[name]]),

         # user tibbles
         "editors" = as_user(content[[name]]),
         "creator" = as_user(content[[name]]),
         "modifier" = as_user(content[[name]]),

         # form elements
         "elements" = as_element(content[[name]])
  )
}
