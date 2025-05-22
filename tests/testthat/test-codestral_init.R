library(codestral)

test_that("codestral_init sets environment variables correctly", {
  # Save original environment variables to restore later
  original_mistral_apikey <- Sys.getenv("R_MISTRAL_APIKEY")
  original_codestral_apikey <- Sys.getenv("R_CODESTRAL_APIKEY")
  original_fim_model <- Sys.getenv("R_CODESTRAL_FIM_MODEL")
  original_chat_model <- Sys.getenv("R_CODESTRAL_CHAT_MODEL")
  original_mamba_model <- Sys.getenv("R_CODESTRAL_MAMBA_MODEL")
  original_temperature <- Sys.getenv("R_CODESTRAL_TEMPERATURE")
  original_max_tokens_fim <- Sys.getenv("R_CODESTRAL_MAX_TOKENS_FIM")
  original_max_tokens_chat <- Sys.getenv("R_CODESTRAL_MAX_TOKENS_CHAT")
  original_role_content <- Sys.getenv("R_CODESTRAL_ROLE_CONTENT")
  
  # Create mock API keys (32 characters)
  mock_mistral_apikey <- paste0(rep("a", 32), collapse = "")
  mock_codestral_apikey <- paste0(rep("b", 32), collapse = "")
  
  # Test with custom values
  expect_message(
    codestral_init(
      mistral_apikey = mock_mistral_apikey,
      codestral_apikey = mock_codestral_apikey,
      fim_model = "custom-fim-model",
      chat_model = "custom-chat-model",
      mamba_model = "custom-mamba-model",
      temperature = 0.7,
      max_tokens_FIM = 200,
      max_tokens_chat = "300",
      role_content = "Custom role content"
    ),
    "Initialization of codestral completed"
  )
  
  # Check that environment variables were set correctly
  expect_equal(Sys.getenv("R_MISTRAL_APIKEY"), mock_mistral_apikey)
  expect_equal(Sys.getenv("R_CODESTRAL_APIKEY"), mock_codestral_apikey)
  expect_equal(Sys.getenv("R_CODESTRAL_FIM_MODEL"), "custom-fim-model")
  expect_equal(Sys.getenv("R_CODESTRAL_CHAT_MODEL"), "custom-chat-model")
  expect_equal(Sys.getenv("R_CODESTRAL_MAMBA_MODEL"), "custom-mamba-model")
  expect_equal(Sys.getenv("R_CODESTRAL_TEMPERATURE"), "0.7")
  expect_equal(Sys.getenv("R_CODESTRAL_MAX_TOKENS_FIM"), "200")
  expect_equal(Sys.getenv("R_CODESTRAL_MAX_TOKENS_CHAT"), "300")
  expect_equal(Sys.getenv("R_CODESTRAL_ROLE_CONTENT"), "Custom role content")
  
  # Test with default role_content (NULL)
  expect_message(
    codestral_init(
      mistral_apikey = mock_mistral_apikey,
      codestral_apikey = mock_codestral_apikey,
      role_content = NULL
    ),
    "Initialization of codestral completed"
  )
  
  # Check that default role_content was set
  expect_true(nchar(Sys.getenv("R_CODESTRAL_ROLE_CONTENT")) > 0)
  
  # Test with invalid API keys
  expect_error(
    codestral_init(
      mistral_apikey = "too_short",
      codestral_apikey = mock_codestral_apikey
    ),
    "This apikey is not a valid key"
  )
  
  expect_error(
    codestral_init(
      mistral_apikey = mock_mistral_apikey,
      codestral_apikey = "too_short"
    ),
    "This apikey is not a valid key"
  )
  
  # Restore original environment variables
  Sys.setenv(R_MISTRAL_APIKEY = original_mistral_apikey)
  Sys.setenv(R_CODESTRAL_APIKEY = original_codestral_apikey)
  Sys.setenv(R_CODESTRAL_FIM_MODEL = original_fim_model)
  Sys.setenv(R_CODESTRAL_CHAT_MODEL = original_chat_model)
  Sys.setenv(R_CODESTRAL_MAMBA_MODEL = original_mamba_model)
  Sys.setenv(R_CODESTRAL_TEMPERATURE = original_temperature)
  Sys.setenv(R_CODESTRAL_MAX_TOKENS_FIM = original_max_tokens_fim)
  Sys.setenv(R_CODESTRAL_MAX_TOKENS_CHAT = original_max_tokens_chat)
  Sys.setenv(R_CODESTRAL_ROLE_CONTENT = original_role_content)
})

test_that("codestral_init returns invisible 0", {
  # Create mock API keys (32 characters)
  mock_mistral_apikey <- paste0(rep("a", 32), collapse = "")
  mock_codestral_apikey <- paste0(rep("b", 32), collapse = "")
  
  # Test return value
  expect_equal(
    suppressMessages(
      codestral_init(
        mistral_apikey = mock_mistral_apikey,
        codestral_apikey = mock_codestral_apikey
      )
    ),
    0
  )
})