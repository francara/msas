package msas.scalafx

import javafx.event.EventHandler
import javafx.event.ActionEvent
import javafx.scene.control.Button
import javafx.scene.input.MouseEvent
import scalafx.Includes.jfxButton2sfx
import scalafx.Includes.jfxDuration2sfx
import scalafx.Includes.observableList2ObservableBuffer
import scalafx.animation.FadeTransition.sfxFadeTransition2jfx
import scalafx.animation.FadeTransition
import scalafx.application.JFXApp
import scalafx.scene.layout.StackPane.sfxStackPane2jfx
import scalafx.scene.layout.StackPane
import scalafx.scene.Scene
import scalafx.stage.Stage
import scalafx.util.Duration

object FadeInOut extends JFXApp {

    stage = new Stage {
        title = "Fade Button In and Out"
        width = 300
        height = 250
    }

    private val btn = new Button {
        setText("Say 'Hello World'")
        setOnAction(new EventHandler[ActionEvent] {
            def handle(event: ActionEvent) {
                println("Hello World")
            }
        })
    }

    private val theRoot = new StackPane {
        children += btn
    }

    private val theScene = new Scene {
        root = theRoot

        onMouseEntered = new EventHandler[MouseEvent] {
            def handle(mouseEvent: MouseEvent) {
                val fadeTransition = new FadeTransition {
                    duration = Duration(500)
                    node = btn
                    fromValue = 0
                    toValue = 1
                }
                fadeTransition.play()
            }
        }

        onMouseExited = new EventHandler[MouseEvent] {
            def handle(mouseEvent: MouseEvent) {
                val fadeTransition = new FadeTransition {
                    duration = Duration(500)
                    node = btn
                    fromValue = 1
                    toValue = 0
                }
                fadeTransition.play()
            }
        }

    }

    stage.scene = theScene

}