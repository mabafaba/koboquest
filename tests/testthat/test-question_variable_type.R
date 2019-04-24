context("question_type()")
source("./utilities_for_t.R")

# ---> no longer relevant, as question_type does not exist if questionnaire is not loaded
# test_that("question_type works without the questionnaire",{
#   data<-koboquest:::read.csv.auto.sep(paste0(example.data.path("example1"),"data.csv"))
#   example<-load.example("example1",with_questionnaire = FALSE)
#   expect_equal(question_type(example$tf$select_one[1], data), "select_one")
#   expect_equal(question_type(example$tf$select_multiple[1], data), "select_one")
#   expect_equal(question_type(example$tf$numeric_NA_heavy[1], data), "numeric")
#   expect_warning(question_type(example$tf$select_multiple[1], data))
#   expect_error(question_type(example$tf$numeric_NA_heavy[1], data,from.questionnaire = F,from.data = T))
#   expect_error(question_type(example$tf$fake[1], data))
#   expect_error(question_type(variable.name = example$tf$fake[1],
#                              data = data,
#                              from.questionnaire = F,
#                              from.data = T))
# })

# test_that("question_variable_type: errors",{
#   example1<-load.example("example1")
#   good_parameters<-list(
#     example.data.path("example1"),
#     questions = paste0(example1$path,"kobo_questions.csv"),
#     choices = paste0(example1$path,"kobo_choices.csv"),
#     choices.label.column.to.use = example1$choice.label.column.to.use)
# })


test_that("question_in_questionnaire returns FALSE unless question is in the questionnaire",{
  example<-load.example("example1")

  expect_equal(typeof(example$questionnaire$question_in_questionnaire),"closure")
  expect_equal(example$questionnaire$question_in_questionnaire(example$tf$select_one[1]),TRUE)
  expect_true(example$questionnaire$question_in_questionnaire(example$tf$select_one_NA_heavy[1]))
  expect_error(example$questionnaire$question_in_questionnaire(example$tf$select_one)) #list input instead of string + too many inputs
  expect_error(example$questionnaire$question_in_questionnaire(example$tf$`NA`[1], questionnaire))
  expect_false(example$questionnaire$question_in_questionnaire(example$tf$fake[1]))
  is_questionnaire_loaded <- function(){return(FALSE)}

} )

