package usp.cognitio.msas.view.ui
import scalafx.scene.layout.StackPane
import scalafx.scene.text.Text
import scalafx.scene.text.Font
import javafx.scene.paint.Color
import javafx.scene.text.FontWeight
import javafx.scene.shape.Rectangle
import javafx.scene.paint.Stop
import scalafx.scene.paint.LinearGradient
import scalafx.scene.paint.CycleMethod

case class Lb(txt: String) extends Text(txt) { font = Font.font("Arial", FontWeight.BOLD, 12) }

case class Bt(lb: String) extends StackPane {
  val icon = new Rectangle(30.0, 25.0)
  icon.setFill(new LinearGradient(0.0, 0.0, 0.0, 1.0, true, CycleMethod.NO_CYCLE,
    List(
      new Stop(0, Color.web("#4977A3")),
      new Stop(0.5, Color.web("#B0C6DA")),
      new Stop(1, Color.web("#9CB6CF")))).delegate);

  icon.setStroke(Color.web("#D0E6FA"));
  icon.setArcHeight(3.5);
  icon.setArcWidth(3.5);

  val text = new Text(lb);
  text.setFont(Font.font("Verdana", FontWeight.BOLD, 18));
  text.setFill(Color.WHITE);
  text.setStroke(Color.web("#7080A0"));

  this.getChildren().addAll(icon, text);

}