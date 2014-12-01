#####

# !!!!!
# Load test data --> testthat needs this data for tests!
load("adehabitatLT_extendedExData.RData")

#wd <- getwd()
#load(paste(wd, "/tests/testthat/", "adehabitatLT_extendedExData.RData", sep=""), verbose=T)
#data(puechcirc)
#data(puechabonsp)

#####


#===============================================================================
#
context("ltrajCoercion.R: 1. Coercion of ltraj objects to Track objects")
#
#===============================================================================

#-------------------------------------------------------------------------------
# 1.1 Test coercion from ltraj to Track
#-------------------------------------------------------------------------------

test_that("1.1 coerce_ltraj2Track", {
  
  ##
  # 1.1.1 Test as(puechcirc,"Track") (ltraj object from example data to Track object)
  expect_that(ltrajTrack1 <- as(puechcirc, "Track"), throws_error("[not,nicht] TRUE"))
  
  # Test class
  expect_that(puechcirc, is_a("ltraj"))
  
  # Test length
  expect_that(length(puechcirc), equals(3))
  
  ##
  # 1.1.2 Test as(puechcirc1,"Track") (ltraj object with length 1)
  ltrajTrack2 <- as(puechcirc1, "Track")
  
  # Test class
  expect_that(puechcirc1, is_a("ltraj"))
  expect_that(ltrajTrack2, is_a("Track"))
  
  # Test ...
  #expect_that(...
  
  
})

  