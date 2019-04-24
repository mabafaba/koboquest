source("./utilities_for_t.R")
context("load_questionnaire")

test_that("no questionnaire loaded reactions",{
  expect_false(question_in_questionnaire("population_group"))
  expect_false(question_in_questionnaire(NULL))
  expect_error(question_is_select_multiple("asdf"))
  expect_error(question_is_select_one("asdf"))
  testframe<-data.frame(a=runif(100),b=runif(100))
  expect_warning(no_questionnaire_none_skipped<-question_is_skipped(testframe,"a"))
  expect_identical(no_questionnaire_none_skipped,rep(F,100))


})

test_that("load_questionnaire: fail on bad input",{
  example1<-load.example("example1")

  #1
  expect_error(load_questionnaire())

  good_parameters<-list(
                        example.data.path("example1"),
                        questions = paste0(example1$path,"kobo_questions.csv"),
                        choices = paste0(example1$path,"kobo_choices.csv"),
                        choices.label.column.to.use = example1$choice.label.column.to.use)


  bad_parameters<-rep(list(good_parameters),10)
  bad_parameters[[1]]$questions<-"not_a_file"
  bad_parameters[[2]]$questions<-NA
  bad_parameters[[3]]$choices<-"not_a_file"
  bad_parameters[[4]]$choices<-NA
  bad_parameters[[5]]$choices.label.column.to.use<-NA
  # 2-9
  expect_error(do.call(load_questionnaire,c(bad_paramters[[1]],data=list(example1$data))))
  expect_error(do.call(load_questionnaire,c(bad_paramters[[1]],data=list(example1$data))))
  expect_error(do.call(load_questionnaire,c(bad_paramters[[1]],data=list(example1$data))))
  expect_error(do.call(load_questionnaire,c(bad_paramters[[1]],data=list(example1$data))))
  expect_error(do.call(load_questionnaire,c(bad_paramters[[5]],data=list(example1$data))))
  expect_error(do.call(load_questionnaire,c(good_paramters[[5]],data=NA)))
  expect_error(do.call(load_questionnaire,c(good_paramters[[5]],data=NULL)))
  expect_error(do.call(load_questionnaire,c(good_paramters[[5]],data=c(1:100))))

})

# # question_is_skipped
# # question_is_categorical
# # question_is_numeric
# # question_is_select_one
#
# # question_is_select_multiple
# # question_get_choice_labels
# # question_get_question_label
# # question_is_sm_choice
# # is_questionnaire_loaded
# # question_in_questionnaire
# # question_is_skipped
# #
