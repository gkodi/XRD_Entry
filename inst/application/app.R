# My first complete app
#'@title Graphical User Interface for XRD data entry.
#'
#'@description This package provide a graphical user interface to enter xrd data easily into the database.
#'@param ui shine app
#'@export
#'@keywords
#'@seealso
#'@return
#'@aliases
#'@examples xrd_db(ui)

Logged = FALSE;
my_username <- list("Guest")
my_password <- list("1234")

GetTableMetadata <- function() {
  fields <- c(id = "Id",
              name = "name",
              anal = "anal",
              anal_ID = "anal_ID",
              quar = "quar",
              quar_ID = "quar_ID",
              cal = "cal",
              cal_ID = "cal_ID",
              arag = "arag",
              arag_ID = "arag_ID",
              dolo = "dolo",
              dolo_ID = "dolo_ID",
              orth = "orth",
              orth_ID = "orth_ID",
              anorth = "anorth",
              anorth_ID = "anorth_ID",
              plagi = "plagi",
              plagi_ID = "plagi_ID",
              mont = "mont",
              mont_ID = "mont_ID",
              ill = "ill",
              ill_ID = "ill_ID",
              py = "py",
              py_ID = "py_ID",
              alb = "alb",
              alb_ID = "alb_ID",
              mus = "mus",
              mus_ID = "mus_ID",
              anothi = "anothi",
              anothi_ID = "anothi_ID",
              iron = "iron",
              iron_ID = "iron_ID",
              paly = "paly",
              paly_ID = "paly_ID",
              chab = "chab",
              chab_ID = "chab_ID",
              phil = "phil",
              phil_ID = "phil_ID",
              harm = "harm",
              harm_ID = "harm_ID",
              clin = "clin",
              clin_ID = "clin_ID",
              erio = "erio",
              erio_ID = "erio_ID",
              sani = "sani",
              sani_ID = "sani_ID",
              hali = "hali",
              hali_ID = "hali_ID",
              gyp = "gyp",
              gyp_ID = "gyp_ID",
              andr = "andr",
              andr_ID = "andr_ID",
              nont = "nont",
              nont_ID = "nont_ID",
              any1 = "any1",
              any1_ID = "any1_ID",
              any2 = "any2",
              any2_ID = "any2_ID")


  result <- list(fields = fields)
  return (result)
}
# Find the next ID of a new record
# (in mysql, this could be done by an incremental index)
GetNextId <- function() {
  if (exists("responses") && nrow(responses) > 0) {
    max(as.integer(rownames(responses))) + 1
  } else {
    return (1)
  }
}

#C
CreateData <- function(data) {

  data <- CastData(data)
  rownames(data) <- GetNextId()
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

#R
ReadData <- function() {
  if (exists("responses")) {
    responses
  }
}



#U
UpdateData <- function(data) {
  data <- CastData(data)
  responses[row.names(responses) == row.names(data), ] <<- data
}

#D
DeleteData <- function(data) {
  responses <<- responses[row.names(responses) != unname(data["id"]), ]
}




# Cast from Inputs to a one-row data.frame
CastData <- function(data) {
  datar <- data.frame(Name = data["name"],
                      anal = data["anal"],
                      anal_ID = data["anal_ID"],
                      quar = data["quar"],
                      quar_ID = data["quar_ID"],
                      cal = data["cal"],
                      cal_ID = data["cal_ID"],
                      arag = data["arag"],
                      arag_ID = data["arag_ID"],
                      dolo = data["dolo"],
                      dolo_ID = data["dolo_ID"],
                      orth = data["orth"],
                      orth_ID = data["orth_ID"],
                      anorth = data["anorth"],
                      anorth_ID = data["anorth_ID"],
                      plagi = data["plagi"],
                      plagi_ID = data["plagi_ID"],
                      mont = data["mont"],
                      mont_ID = data["mont_ID"],
                      ill = data["ill"],
                      ill_ID = data["ill_ID"],
                      py = data["py"],
                      py_ID = data["py_ID"],
                      alb = data["alb"],
                      alb_ID = data["alb_ID"],
                      mus = data["mus"],
                      mus_ID = data["mus_ID"],
                      anothi = data["anothi"],
                      anothi_ID = data["anothi_ID"],
                      iron = data["iron"],
                      iron_ID = data["iron_ID"],
                      paly = data["paly"],
                      paly_ID = data["paly_ID"],
                      chab = data["chab"],
                      chab_ID = data["chab_ID"],
                      phil = data["phil"],
                      phil_ID = data["phil_ID"],
                      harm = data["harm"],
                      harm_ID = data["harm_ID"],
                      clin = data["clin"],
                      clin_ID = data["clin_ID"],
                      erio = data["erio"],
                      erio_ID = data["erio_ID"],
                      sani = data["sani"],
                      sani_ID = data["sani_ID"],
                      hali = data["hali"],
                      hali_ID = data["hali_ID"],
                      gyp = data["gyp"],
                      gyp_ID = data["gyp_ID"],
                      andr = data["andr"],
                      andr_ID = data["andr_ID"],
                      nont = data["nont"],
                      nont_ID = data["nont_ID"],
                      any1 = data["any1"],
                      any1_ID = data["any1_ID"],
                      any2 = data["any2"],
                      any2_ID = data["any2_ID"],
                      stringsAsFactors = FALSE)

  rownames(datar) <- data["id"]
  return (datar)
}

# Return an empty, new record
CreateDefaultRecord <- function() {
  mydefault <- CastData(list(id = "0", name = "", anal = "0", anal_ID = "NA",quar = "0", quar_ID = "NA",cal = "0", cal_ID = "NA",arag = "0", arag_ID = "NA",dolo = "0", dolo_ID = "NA",orth = "0", orth_ID = "NA",anorth = "0", anorth_ID = "NA",plagi = "0", plagi_ID = "NA",mont = "0", mont_ID = "NA",ill = "0", ill_ID = "NA", py = "0", py_ID = "NA", alb = "0", alb_ID = "NA", mus = "0", mus_ID = "NA", anothi = "0", anothi_ID = "NA", iron = "0", iron_ID = "NA", paly = "0", paly_ID = "NA", chab = "0", chab_ID = "NA", phil = "0", phil_ID = "NA", harm = "0", harm_ID = "NA", clin = "0", clin_ID = "NA", erio = "0", erio_ID = "NA", sani = "0", sani_ID = "NA", hali = "0", hali_ID = "NA", gyp = "0", gyp_ID = "NA", andr = "0", andr_ID = "NA", nont = "0", nont_ID = "NA", any1 = "0", any1_ID = "NA", any2 = "0", any2_ID = "NA"))
  return (mydefault)
}

# Fill the input fields with the values of the selected record in the table
UpdateInputs <- function(data, session) {
  updateTextInput(session, "id", value = unname(rownames(data)))
  updateTextInput(session, "name", value = unname(data["name"]))
  updateTextInput(session, "anal", value = unname(data["anal"]))
  updateTextInput(session, "anal_ID", value = unname(data["anal_ID"]))
  updateTextInput(session, "quar", value = unname(data["quar"]))
  updateTextInput(session, "quar_ID", value = unname(data["quar_ID"]))
  updateTextInput(session, "cal", value = unname(data["cal"]))
  updateTextInput(session, "cal_ID", value = unname(data["cal_ID"]))
  updateTextInput(session, "arag", value = unname(data["arag"]))
  updateTextInput(session, "arag_ID", value = unname(data["arag_ID"]))
  updateTextInput(session, "dolo", value = unname(data["dolo"]))
  updateTextInput(session, "dolo_ID", value = unname(data["dolo_ID"]))
  updateTextInput(session, "orth", value = unname(data["orth"]))
  updateTextInput(session, "orth_ID", value = unname(data["orth_ID"]))
  updateTextInput(session, "anorth", value = unname(data["anorth"]))
  updateTextInput(session, "anorth_ID", value = unname(data["anorth_ID"]))
  updateTextInput(session, "plagi", value = unname(data["plagi"]))
  updateTextInput(session, "plagi_ID", value = unname(data["plagi_ID"]))
  updateTextInput(session, "mont", value = unname(data["mont"]))
  updateTextInput(session, "mont_ID", value = unname(data["mont_ID"]))
  updateTextInput(session, "ill", value = unname(data["ill"]))
  updateTextInput(session, "ill_ID", value = unname(data["ill_ID"]))
  updateTextInput(session, "py", value = unname(data["py"]))
  updateTextInput(session, "py_ID", value = unname(data["py_ID"]))
  updateTextInput(session, "alb", value = unname(data["alb"]))
  updateTextInput(session, "alb_ID", value = unname(data["alb_ID"]))
  updateTextInput(session, "mus", value = unname(data["mus"]))
  updateTextInput(session, "mus_ID", value = unname(data["mus_ID"]))
  updateTextInput(session, "anothi", value = unname(data["anothi"]))
  updateTextInput(session, "anothi_ID", value = unname(data["anothi_ID"]))
  updateTextInput(session, "iron", value = unname(data["iron"]))
  updateTextInput(session, "iron_ID", value = unname(data["iron_ID"]))
  updateTextInput(session, "paly", value = unname(data["paly"]))
  updateTextInput(session, "paly_ID", value = unname(data["paly_ID"]))
  updateTextInput(session, "chab", value = unname(data["chab"]))
  updateTextInput(session, "chab_ID", value = unname(data["chab_ID"]))
  updateTextInput(session, "phil", value = unname(data["phil"]))
  updateTextInput(session, "phil_ID", value = unname(data["phil_ID"]))
  updateTextInput(session, "harm", value = unname(data["harm"]))
  updateTextInput(session, "harm_ID", value = unname(data["harm_ID"]))
  updateTextInput(session, "clin", value = unname(data["clin"]))
  updateTextInput(session, "clin_ID", value = unname(data["clin_ID"]))
  updateTextInput(session, "erio", value = unname(data["erio"]))
  updateTextInput(session, "erio_ID", value = unname(data["erio_ID"]))
  updateTextInput(session, "sani", value = unname(data["sani"]))
  updateTextInput(session, "sani_ID", value = unname(data["sani_ID"]))
  updateTextInput(session, "hali", value = unname(data["hali"]))
  updateTextInput(session, "hali_ID", value = unname(data["hali_ID"]))
  updateTextInput(session, "gyp", value = unname(data["gyp"]))
  updateTextInput(session, "gyp_ID", value = unname(data["gyp_ID"]))
  updateTextInput(session, "andr", value = unname(data["andr"]))
  updateTextInput(session, "andr_ID", value = unname(data["andr_ID"]))
  updateTextInput(session, "nont", value = unname(data["nont"]))
  updateTextInput(session, "nont_ID", value = unname(data["nont_ID"]))
  updateTextInput(session, "any1", value = unname(data["any1"]))
  updateTextInput(session, "any1_ID", value = unname(data["any1_ID"]))
  updateTextInput(session, "any2", value = unname(data["any2"]))
  updateTextInput(session, "any2_ID", value = unname(data["any2_ID"]))
}

ui1 <- function(){
  tagList(
    div(id = "login",
        wellPanel(textInput("userName", "Username"),
                  passwordInput("passwd", "Password"),
                  br(),actionButton("Login", "Log in"))),
    tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}

ui <- (htmlOutput("page"))
server <- function(input, output,session) {

  USER <- reactiveValues(Logged = Logged)

  observe({
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(my_username == Username)
          Id.password <- which(my_password == Password)
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username == Id.password) {
              USER$Logged <- TRUE
            }
          }
        }
      }
    }
  })
  observe({
    if (USER$Logged == FALSE) {

      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",ui1())))
      })
    }
    if (USER$Logged == TRUE)
    {
      output$page <- renderUI({


        ui2 <- fluidPage( titlePanel("Interactive XRD Data Entry Platform."),h6("gayantha@uwm.edu"),
                          #use shiny js to disable the ID field
                          shinyjs::useShinyjs(),

                          #input fields
                          tags$hr(),
                          textInput("name", "Sample Name", ""),
                          hr(),
                          fluidRow(column(1,
                                          selectInput("anal", "Analcime:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4")),
                                          selectInput("quar", "Quartz:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4")),
                                          selectInput("cal", "Calcite:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4")),
                                          selectInput("arag", "Aragonite:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4")),
                                          selectInput("dolo", "Dolomite:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4")),
                                          selectInput("orth", "Ortho:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4")),
                                          selectInput("anorth", "Anortho:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4"))),
                                   column(2,

                                          selectInput("anal_ID","Anal_ID:", c("NA" = "NA", "00-007-0340"= "00-007-0340", "00-002-0417" = "00-002-0417", "00-041-1478" = "00-041-1478")),
                                          selectInput("quar_ID","Quar_ID:", c("NA" = "NA", "00-052-0144"= "00-052-0144", "00-085-0460"= "00-085-0460", "01-086-1560" = "01-086-1560")),
                                          selectInput("cal_ID","Cal_ID:", c("NA" = "NA", "01-071-3699"= "01-071-3699", "01-072-4582"= "01-072-4582", "00-005-0586" = "00-005-0586")),
                                          selectInput("arag_ID","Arag_ID:", c("NA" = "NA", "00-003-0893"= "00-003-0893", "01-075-2230"= "01-075-2230", "00-041-1475" = "00-041-1475")),
                                          selectInput("dolo_ID","Dolo_ID:", c("NA" = "NA", "01-084-2065"= "01-084-2065", "00-036-0426"= "00-036-0426", "01-079-1345" = "01-079-1345")),
                                          selectInput("orth_ID","Orth_ID:", c("NA" = "NA", "01-086-0439"= "01-086-0439", "01-076-0825"= "01-076-0825", "00-019-0002" = "00-019-0002")),
                                          selectInput("anorth_ID","Anorth_ID:", c("NA" = "NA", "00-009-0478"= "00-009-0478", "01-076-0803"= "01-076-0803", "01-075-1630" = "01-075-1630"))),

                                   column(1,
                                          selectInput("plagi", "Plagio:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4")),
                                          selectInput("mont", "Montmo:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4")),
                                          selectInput("ill", "Illite:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4")),
                                          selectInput("py", "Pyrite:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4")),
                                          selectInput("alb", "Albite:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4")),
                                          selectInput("mus", "Muscovite:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4")),
                                          selectInput("anothi", "Anothite:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4"))),
                                   column(2,
                                          selectInput("plagi_ID","Plagi_ID:", c("NA" = "NA", "01-083-1371"= "01-083-1371", "01-083-1417"= "01-083-1417", "01-083-1367" = "01-083-1367")),
                                          selectInput("mont_ID","Mont_ID:", c("NA" = "NA", "00-003-0015"= "00-003-0015", "00-012-0204"= "00-012-0204", "00-002-0037" = "00-002-0037")),
                                          selectInput("ill_ID","Ill_ID:", c("NA" = "NA", "00-009-0343"= "00-009-0343", "00-015-0603"= "00-015-0603", "00-025-0001" = "00-025-0001")),
                                          selectInput("py_ID","Py_ID:", c("NA" = "NA", "00-042-1340"= "00-042-1340", "01-071-0053"= "01-071-0053", "01-071-3840" = "01-071-3840")),
                                          selectInput("alb_ID","Alb_ID:", c("NA" = "NA", "01-083-1939"= "01-083-1939", "01-089-6423"= "01-089-6423", "00-041-1480" = "00-041-1480")),
                                          selectInput("mus_ID","Mus_ID:", c("NA" = "NA", "00-002-0055"= "00-002-0055", "00-002-0993"= "00-002-0993", "01-089-5401" = "01-089-5401")),
                                          selectInput("anothi_ID","Anothi_ID:", c("NA" = "NA", "00-020-0528"= "00-020-0528", "01-075-1587"= "01-075-1587", "00-018-1202" = "00-018-1202"))),
                                   column(1,
                                          selectInput("iron", "Iron Ox:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4")),
                                          selectInput("paly", "Palygor:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4")),
                                          selectInput("chab", "Chabazite:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4")),
                                          selectInput("phil", "Phillips:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4")),
                                          selectInput("harm", "Harmotome:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4")),
                                          selectInput("clin", "Clinopti:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4")),
                                          selectInput("erio", "Erionite:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4"))),
                                   column(2,
                                          selectInput("iron_ID","Iron_ID:", c("NA" = "NA", "00-001-0662"= "00-001-0662", "00-002-1047"= "00-002-1047", "00-026-1223" = "00-026-1223")),
                                          selectInput("paly_ID","Paly_ID:", c("NA" = "NA", "00-005-0099"= "00-005-0099", "00-020-0688"= "00-020-0688", "01-082-1872" = "01-082-1872")),
                                          selectInput("chab_ID","Chab_ID:", c("NA" = "NA", "00-053-1177"= "00-053-1177", "00-034-0137"= "00-034-0137", "01-086-1548" = "01-073-1548")),
                                          selectInput("phil_ID","Phil_ID:", c("NA" = "NA", "00-046-1427"= "00-046-1427", "00-039-1375"= "00-039-1375", "01-073-1419" = "01-073-1419")),
                                          selectInput("harm_ID","Harm_ID:", c("NA" = "NA", "00-020-0468"= "00-020-0468", "00-012-0687"= "00-012-0687", "00-025-0855" = "00-025-0855")),
                                          selectInput("clin_ID","Clin_ID:", c("NA" = "NA", "00-025-1349"= "00-025-1349", "01-083-1261"= "01-083-1261", "00-024-0319" = "00-024-0319")),
                                          selectInput("erio_ID","Erio_ID:", c("NA" = "NA", "00-022-0854"= "00-022-0854", "00-039-1379"= "00-039-1379", "01-088-1224" = "01-088-1224"))),
                                   column(1,
                                          selectInput("sani", "Sanidine:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4")),
                                          selectInput("hali", "Halite:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4")),
                                          selectInput("gyp", "Gypsum:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4")),
                                          selectInput("andr", "Andradi:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4")),
                                          selectInput("nont", "Nontroni:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4")),
                                          selectInput("any1", "Other1:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4")),
                                          selectInput("any2", "Other2:", c("NA" = "0", "X" = "1", "XX" = "2", "XXX" = "3", "XXXX" = "4"))),
                                   column(2,
                                          selectInput("sani_ID","Sanid_ID:", c("NA" = "NA", "01-087-0684"= "01-087-0684", "00-010-0357"= "00-010-0357", "01-083-1657" = "01-083-1657")),
                                          selectInput("hali_ID","Hali_ID:", c("NA" = "NA", "01-071-3741"= "01-071-3741", "01-075-0305"= "01-075-0305", "00-005-0628" = "00-005-0628")),
                                          selectInput("gyp_ID","Gyp_ID:", c("NA" = "NA", "01-074-1433"= "01-074-1433", "01-070-7008"= "01-070-7008", "00-006-0047" = "00-006-0047")),
                                          selectInput("andr_ID","Andr_ID:", c("NA" = "NA", "01-085-1370"= "01-085-1370", "01-089-7563"= "01-089-7563", "01-074-1559" = "01-074-1559")),
                                          selectInput("nont_ID","Nont_ID:", c("NA" = "NA", "00-002-0017"= "00-002-0017", "00-002-0020"= "00-002-0020", "00-002-0026" = "00-002-0026")),
                                          textInput("any1_ID", "Mineral ID", ""),
                                          textInput("any2_ID", "Mineral ID", ""))),
                          actionButton("submit", "Submit"),
                          actionButton("new", "New"),
                          actionButton("delete", "Delete"),
                          shinyjs::disabled(textInput("id", "Id", "0")),
                          hr(),

                          #data table
                          DT::dataTableOutput("responses", width = 900))

      })
      #}




      #server <- function(input, output, session) {

      # input fields are treated as a group
      formData <- reactive({
        sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
      })

      # Click "Submit" button -> save data
      observeEvent(input$submit, {
        if (input$id != "0") {
          UpdateData(formData())
        } else {
          CreateData(formData())
          UpdateInputs(CreateDefaultRecord(), session)
        }
      }, priority = 1)

      # Press "New" button -> display empty record
      observeEvent(input$new, {
        UpdateInputs(CreateDefaultRecord(), session)
      })

      # Press "Delete" button -> delete from data
      observeEvent(input$delete, {
        DeleteData(formData())
        UpdateInputs(CreateDefaultRecord(), session)
      }, priority = 1)

      # Select row in table -> show details in inputs
      observeEvent(input$responses_rows_selected, {
        if (length(input$responses_rows_selected) > 0) {
          data <- ReadData()[input$responses_rows_selected, ]
          UpdateInputs(data, session)}

      })

      # display table
      output$responses <- DT::renderDataTable({
        #update after submit is clicked
        input$submit
        #fileName <- sprintf("%s_%s.csv",as.integer(Sys.time()), digest::digest(responses))
        #write.csv(responses, file = file.path(myDir, fileName), row.names = FALSE, quote = TRUE)
        write.csv(responses, "backup.csv")
        #update after delete is clicked
        input$delete
        ReadData()
      }, server = FALSE, selection = "single",
      colnames = unname(GetTableMetadata()$fields)[-1]
      )
    }
  }
  )}

shinyApp(ui, server)
