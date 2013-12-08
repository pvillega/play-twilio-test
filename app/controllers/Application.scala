package controllers

import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models.TwilioCredentials
import play.api.libs.iteratee.{Input, Done}

object Application extends Controller with TwilioAccess {

  val TOKEN: String = "token"
  val SID: String = "sid"

  val credentialsForm: Form[TwilioCredentials] = Form(
    mapping(
      "sid" -> nonEmptyText,
      "token" -> nonEmptyText
    )(TwilioCredentials.apply)(TwilioCredentials.unapply)
  )

  def index = Action {
    implicit request =>
      Ok(views.html.index(credentialsForm)).withNewSession
  }

  def getCredentials = Action {
    implicit request =>
      credentialsForm.bindFromRequest.fold(
        errors => BadRequest(views.html.index(errors)),
        credentials => Redirect(routes.Application.testTwilio).withSession(SID -> credentials.sid, TOKEN -> credentials.token)
      )
  }

  def testTwilio = hasCredentials {
    (sid, token) =>
      implicit request =>
        Ok(views.html.twilio(sid))
  }

}

trait TwilioAccess {

  private def sid(request: RequestHeader) = request.session.get(Application.SID)

  private def auth(request: RequestHeader) = request.session.get(Application.TOKEN)

  private def onUnauthorized(request: RequestHeader) = Results.Redirect(routes.Application.index).flashing("session" -> "SID or Auth token missing in session")

  def hasCredentials(f: => (String, String) => Request[AnyContent] => Result) = credentialsPresent(onUnauthorized) {
    (sid, token) =>
      Action(request => f(sid, token)(request))
  }

  def credentialsPresent[A](onUnauthorized: RequestHeader => SimpleResult)(action: (String, String) => EssentialAction): EssentialAction = {
    EssentialAction {
      request =>
        val sid = request.session.get(Application.SID)
        val token = request.session.get(Application.TOKEN)

        if (sid.isEmpty || token.isEmpty)
          Done(onUnauthorized(request), Input.Empty)
        else
          action(sid.get, token.get)(request)
    }
  }

}