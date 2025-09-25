#' Linear Regression Class using QR Decomposition
#'
#' A class for performing linear regression analysis using QR decomposition
#'
#' @param formula A formula object specifying the model
#' @param data A data frame containing the variables
#' @return An object of class linreg containing regression results
#' @importFrom stats na.omit model.matrix model.response model.frame pt median
#' @export
linreg <- function(formula, data) {
  # Remove missing values
  data <- stats::na.omit(data)

  # Create model matrix and response vector
  X <- stats::model.matrix(formula, data)
  y <- stats::model.response(stats::model.frame(formula, data))

  # Get variable names
  variable_names <- colnames(X)

  # Perform QR decomposition of X
  qr_decomp <- qr(X)
  Q <- qr.Q(qr_decomp)  # Orthogonal matrix Q
  R <- qr.R(qr_decomp)  # Upper triangular matrix R

  # Calculate coefficients using QR decomposition: β = R^(-1) Q^T y
  coefficients <- solve(R, t(Q) %*% y)
  coefficients <- as.vector(coefficients)
  names(coefficients) <- variable_names

  # Calculate fitted values: ŷ = Xβ = Q Q^T y
  fitted_values <- as.vector(Q %*% t(Q) %*% y)

  # Calculate residuals: e = y - ŷ
  residuals <- y - fitted_values

  # Degrees of freedom
  n <- nrow(X)
  p <- ncol(X)
  df <- n - p

  # Residual standard error: σ = sqrt(SSE / df)
  rss <- sum(residuals^2)  # Residual sum of squares
  rse <- sqrt(rss / df)    # Residual standard error

  # Calculate variance-covariance matrix using QR decomposition
  R_inv <- solve(R)
  vcov_matrix <- (rse^2) * (R_inv %*% t(R_inv))

  # Standard errors of coefficients (square root of diagonal elements)
  std_errors <- sqrt(diag(vcov_matrix))

  # t-values and p-values
  t_values <- coefficients / std_errors
  p_values <- 2 * stats::pt(abs(t_values), df, lower.tail = FALSE)

  # Create result object
  result <- list(
    coefficients = coefficients,
    fitted_values = fitted_values,
    residuals = residuals,
    formula = formula,
    data = data,
    std_errors = std_errors,
    t_values = t_values,
    p_values = p_values,
    rse = rse,
    df = df,
    qr_decomp = qr_decomp,
    vcov_matrix = vcov_matrix,
    call = match.call()
  )

  class(result) <- "linreg"
  return(result)
}

#' Print method for linreg objects
#'
#' @param x linreg object
#' @param ... additional arguments
#' @export
print.linreg <- function(x, ...) {
  cat("Call:\n")
  cat("linreg(formula = ", deparse(x$formula), ", data = ", deparse(x$call$data), ")\n\n", sep = "")
  cat("Coefficients:\n")
  print(x$coefficients)
}

#' Residuals method for linreg objects
#'
#' @param object linreg object
#' @param ... additional arguments
#' @return Vector of residuals
#' @export
residuals.linreg <- function(object, ...) {
  return(object$residuals)
}

#' Extract residuals from a model
#'
#' @param object A model object
#' @param ... Additional arguments
#' @return Model residuals
#' @export
resid <- function(object, ...) {
  UseMethod("resid")
}

#' Extract residuals from linreg objects
#'
#' @param object A linreg object
#' @param ... Additional arguments
#' @return Residuals from the linear regression model
#' @export
resid.linreg <- function(object, ...) {
  return(object$residuals)
}

#' Prediction Method for Linear Regression Model
#'
#' @description This function extracts predicted values from a fitted linear
#' regression model of class 'linreg'. These are the fitted values based on
#' the model coefficients and input data.
#'
#' @param object An object of class 'linreg' containing the fitted regression model
#' @param ... Additional arguments passed to methods
#'
#' @return A numeric vector containing the predicted (fitted) values
#'
#' @examples
#' # Fit a linear regression model
#' model <- linreg(mpg ~ wt + hp, data = mtcars)
#'
#' # Get predictions
#' predictions <- pred(model)
#'
#' @export
pred <- function(object, ...) {
  UseMethod("pred")
}

#' Pred method for linreg objects
#'
#' @param object linreg object
#' @param ... additional arguments
#' @return Vector of predicted values
#' @export
pred.linreg <- function(object, ...) {
  return(object$fitted_values)
}

#' Coefficients method for linreg objects
#'
#' @param object linreg object
#' @param ... additional arguments
#' @return Named vector of coefficients
#' @export
coef.linreg <- function(object, ...) {
  return(object$coefficients)
}

#' Summary method for linreg objects
#'
#' @param object linreg object
#' @param ... additional arguments
#' @export
summary.linreg <- function(object, ...) {
  # Use the same output format as lm
  cat("Call:\n")
  cat("linreg(formula = ", deparse(object$formula), ", data = ", deparse(object$call$data), ")\n\n", sep = "")

  cat("Coefficients:\n")

  # Create the coefficient matrix
  coef_matrix <- cbind(
    Estimate = object$coefficients,
    `Std. Error` = object$std_errors,
    `t value` = object$t_values,
    `Pr(>|t|)` = object$p_values
  )

  # Use printCoefmat to match the output format of lm
  stats::printCoefmat(coef_matrix,
                      digits = 3,
                      signif.stars = TRUE,
                      signif.legend = FALSE,
                      P.values = TRUE,
                      has.Pvalue = TRUE)

  cat("\nResidual standard error:", format(object$rse, digits = 3), "on", object$df, "degrees of freedom\n")
}

#' Plot method for linreg objects
#'
#' Creates diagnostic plots using ggplot2 as specified in the requirements
#'
#' @param x linreg object
#' @param ... additional arguments
#' @import ggplot2
#' @importFrom stats median
#' @export
plot.linreg <- function(x, ...) {
  # Ensure ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }

  # Create data frame for plotting with proper variable names
  fitted_vals <- x$fitted_values
  residual_vals <- x$residuals

  plot_data <- data.frame(
    fitted = fitted_vals,
    residuals = residual_vals
  )

  # Calculate median residual for reference
  median_residual <- stats::median(residual_vals)

  # Plot 1: Residuals vs Fitted
  p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$fitted, y = .data$residuals)) +
    ggplot2::geom_point(alpha = 0.7, shape = 1, size = 2) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
    ggplot2::geom_hline(yintercept = median_residual, linetype = "dotted", color = "blue", linewidth = 0.8) +
    ggplot2::labs(
      title = "Residuals vs Fitted",
      x = "Fitted values",
      y = "Residuals"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )

  # Plot 2: Scale-Location plot
  # Calculate sqrt(|standardized residuals|) for Scale-Location plot
  standardized_residuals <- residual_vals / x$rse
  sqrt_abs_resid <- sqrt(abs(standardized_residuals))
  plot_data$sqrt_abs_resid <- sqrt_abs_resid

  p2 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$fitted, y = .data$sqrt_abs_resid)) +
    ggplot2::geom_point(alpha = 0.7, shape = 1, size = 2) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 0.8) +
    ggplot2::labs(
      title = "Scale-Location",
      x = "Fitted values",
      y = expression(sqrt(abs("Standardized residuals")))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )

  # Display both plots
  print(p1)
  cat("\n")  # Add some space between plots
  print(p2)

  # Return plots invisibly for further use if needed
  return(invisible(list(
    residuals_vs_fitted = p1,
    scale_location = p2
  )))
}

#' Variance-Covariance Matrix method for linreg objects
#'
#' Returns the variance-covariance matrix of the coefficients using QR decomposition
#'
#' @param object linreg object
#' @param ... additional arguments
#' @return Variance-covariance matrix
#' @export
vcov.linreg <- function(object, ...) {
  return(object$vcov_matrix)
}

#' QR Decomposition information
#'
#' Returns information about the QR decomposition used in the model
#'
#' @param x linreg object
#' @param ... additional arguments
#' @return QR decomposition object
#' @export
qr.linreg <- function(x, ...) {
  return(x$qr_decomp)
}

