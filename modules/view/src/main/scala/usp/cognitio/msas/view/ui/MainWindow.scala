package usp.cognitio.msas.view.ui
import usp.cognitio.msas.view.ui.MainWindowConfigs._
import javafx.scene.layout.HBox
import javafx.geometry.Insets
import scalafx.scene.layout.BorderPane
import scalafx.scene.layout.GridPane
import scalafx.scene.text.Text
import scalafx.scene.control.ComboBox
import scalafx.scene.control.Separator
import scalafx.scene.layout.VBox
import javafx.geometry.Orientation
import javafx.beans.value.ChangeListener
import scalafx.scene.control.CheckBox
import scalafx.scene.control.TextField
import scalafx.scene.text.Font
import javafx.scene.text.FontWeight
import javafx.scene.shape.Rectangle
import javafx.scene.paint.Stop
import javafx.scene.layout.Priority
import javafx.scene.paint.Color
import javafx.geometry.Pos
import scalafx.scene.paint.LinearGradient
import scalafx.scene.paint.CycleMethod
import scalafx.scene.Group
import scalafx.scene.layout.StackPane
import scalafx.scene.control.Button
import javafx.event.ActionEvent
import javafx.event.EventHandler

object MainWindowConfigs {
  val PLANNING_ONCE = "Planning Once";
  val PLANNING_ALWAYS = "Planning Always";
}

abstract case class MainWindow() extends BorderPane {

  def onStart(once: Boolean,
    consumable: Boolean,
    sameTarget: Boolean,
    mean: Double, sigma: Double,
    cmean: Double, csigma: Double,
    phySema: Boolean, socSema: Boolean): Unit
  def onPlan: Unit
  def onCoal: Unit
  def onSema(phySema: Boolean, socSema: Boolean): Unit

  /*
   ********************************************
   *********   ENVIRONMENT SETTINGS   *********
   ********************************************
   */
  /**
   * Planning moment: PLAN ONCE | PLAN ALWAYS.
   */
  val planning = new ComboBox(PLANNING_ONCE :: PLANNING_ALWAYS :: Nil) {
    value = PLANNING_ONCE
  }

  /**
   * Resource consumable: TRUE | FALSE.
   * --
   * A visited cell can not be ocuppied anymore.
   */
  val agResourceConsumable = new CheckBox() {
    selected = true
  }

  val cellResourceConsumable = new CheckBox() {
    selected = false
  }

  /*
   ******************************************
   *********   ALGORITHM SETTINGS   *********
   ******************************************
   */

  /**
   * Same target: TRUE | FALSE
   */
  val sameTarget = new CheckBox() {
    selected = true
  }

  /**
   * Ag Res Mean and Sigma
   */
  val mean = new TextField()
  val sigma = new TextField()

  /**
   * Cell Res.
   */
  val cellmean = new TextField()
  val cellsigma = new TextField()

  /*
   ******************************************
   *********   EXECUTION SETTINGS   *********
   ******************************************
   */

  val phySema = new CheckBox() {
    selected = false
  }
  phySema.onAction = new EventHandler[ActionEvent] {
    def handle(event: ActionEvent) {
      onSema(phySema.selected.getValue(), socSema.selected.getValue())
    }
  }

  val socSema = new CheckBox() {
    selected = false
  }
  socSema.onAction = new EventHandler[ActionEvent] {
    def handle(event: ActionEvent) {
      onSema(phySema.selected.getValue(), socSema.selected.getValue())
    }
  }

  minHeight = 280

  val hbox = createTop()
  this.setTop(hbox);
  this.setCenter(createBody())
  this.setBottom(createBottom())

  protected def createTop(): HBox = {
    val hbox = new HBox();
    hbox.setPadding(new Insets(7, 12, 7, 12));
    hbox.setSpacing(10);
    hbox.setStyle("-fx-background-color: #336699;");

    val stack = new StackPane();
    val bt = Bt("C")
    val plan = Bt("P")

    hbox.getChildren().add(bt);
    hbox.getChildren().add(plan);
    HBox.setHgrow(stack, Priority.ALWAYS); // Give stack any extra space

    return hbox;
  }

  protected def createBody(): VBox = {
    val vbox = new VBox()
    vbox.setSpacing(8)
    val grid = new GridPane {
      hgap = 10; vgap = 10
      padding = new Insets(0, 10, 0, 10)

      add(Lb("Planning moment:"), 1, 1)
      add(planning, 2, 1)
      add(Lb("Ag Rc Consumable:"), 1, 2)
      add(agResourceConsumable, 2, 2)
      add(Lb("Cell Rc Consumable:"), 1, 3)
      add(cellResourceConsumable, 2, 3)
      add(Lb("Same Target:"), 1, 4)
      add(sameTarget, 2, 4)
      add(Lb("Mean:"), 1, 5)
      add(mean, 2, 5)
      add(Lb("Sigma:"), 1, 6)
      add(sigma, 2, 6)
      add(Lb("Cell Mean:"), 1, 7)
      add(cellmean, 2, 7)
      add(Lb("Cell Sigma:"), 1, 8)
      add(cellsigma, 2, 8)
      add(Lb("Phy Sema:"), 1, 9)
      add(phySema, 2, 9)
      add(Lb("Soc Sema:"), 1, 10)
      add(socSema, 2, 10)
    }

    val separator = new Separator()
    separator.setOrientation(Orientation.HORIZONTAL)
    vbox.getChildren().add(separator)
    vbox.getChildren().add(grid)
    return vbox
  }

  protected def createBottom(): HBox = {
    val hbox = new HBox();
    hbox.setPadding(new Insets(7, 12, 7, 12));
    hbox.setSpacing(10);
    hbox.setStyle("-fx-background-color: #8FB5DB;");

    val bt = new Button("Simulate") {
      var _pressed = false
      onAction = new EventHandler[ActionEvent] {
        def handle(event: ActionEvent) {
          _pressed = true
          text = "Stop"
          onStart(
            if (planning.value == PLANNING_ONCE) true else false,
            true, true,
            0.00, 0.00,
            0.00, 0.00,
            phySema.selected.getValue(), socSema.selected.getValue())
        }
      }
    }

    bt.setPrefSize(100, 20);
    hbox.getChildren().addAll(bt);
    hbox.setAlignment(Pos.CENTER_RIGHT);
    return hbox;
  }

}
