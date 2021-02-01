

#'Show Error toastr
#'@noRd
show_toaster <- function(.title, .message){
  
  shinyFeedback::showToast(
    "error",
    title = .title,
    message = .message,
    keepVisible = FALSE,
    .options = list(
      positionClass = "toast-top-center",
      timeOut  = 6000
    )
  )
  
}