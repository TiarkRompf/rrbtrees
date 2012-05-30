/**
 *  Copyright © 2011 Phil Bagwell and Tiark Rompf. This is a prototype implementation, use at your own risk.
 */

package TestVector

import scala.swing.Swing._
import scala.swing._
import scala.swing.event._
import java.awt.{Color, Dimension, Graphics, Graphics2D, Point, geom}
import Vector._


class VectDesc(var L:Int,var R:Int,var SL:Int,var SR:Int,var S:Int,var Rand:Boolean)

object VectorDisplay  extends SimpleSwingApplication {
  //val v=createvec(0,1000)
  val vdesc=new VectDesc(1,0,0,100,100,false) 
  lazy val ui = new Panel{
  
    val itemcnt=2000
    background = Color.white
    preferredSize = (4000,1000)
	val point = new geom.Point2D.Double(10, 11)

	
	override def paintComponent(g: Graphics2D) = {
       super.paintComponent(g)
       if(!vdesc.Rand) Tests.fSplit = (x:Int)=>true else Tests.fSplit = (x:Int)=>false
	   Tests.Rands=12345673
	   val vL=Tests.randTestSet(vdesc.L)
	   //val vL=vLa.sliceR(vdesc.SR)
       val gvL=new GVec[Int](vL)
	   Tests.Rands=12345673
       val vR=Tests.randTest(100,100+vdesc.R,1)
       val gvR=new GVec[Int](vR)
	   val vS=vR.slice(vdesc.SL,vdesc.SR)
	   val vC=vL.conca(vS)

//	   val vC=vL.conca(vR)
//	   val vS=vC.slice(vdesc.SL,vdesc.SR)
	   val gvC=new GVec[Int](vC)

		 g.scale(vdesc.S/200,vdesc.S/200)
		 gvL.drawTrie(g)
	   val (x,y)=gvL.bounds
		 g.translate(x*4/3,0)
		 gvR.drawTrie(g)
         g.translate(-x,y*2/3)
		 gvC.drawTrie(g)         		 
	   }
  }

  def createvec(left:Int,right:Int)={ 
    if((left<0)||(left>right))println("***Test vector Error*** l,r",left,right)
	(left until right).foldLeft(new vec[Int])(_+_)
    }
  
  val fp = new FlowPanel {
   	val VL = new TextField { text = "0"; columns = 5 }
    val VR = new TextField { text = "0"; columns = 5 }
   	val VSL = new TextField { text = "0"; columns = 5 }
    val VSR = new TextField { text = "0"; columns = 5 }
    border = Swing.EmptyBorder(15, 10, 10, 10)

	val Rd= new CheckBox("Random")
	
	object sliderL extends Slider {
        min = 0
        value = 1
        max = 100
        majorTickSpacing = 1
      }

	object sliderR extends Slider {
        min = 0
        value = 1
        max = 100
        majorTickSpacing = 1
      }

	  object sliderSliceL extends Slider {
        min = 0
        value = 1
        max = 100
        majorTickSpacing = 1
      }

	object sliderSliceR extends Slider {
        min = 0
        value = 100
        max = 100
        majorTickSpacing = 1
      }

	  object sliderS extends Slider {
        min = 10
        value = 1
        max = 500
        majorTickSpacing = 1
      }
	  
    contents.append( 
	  new Label("Zoom"),sliderS,Rd,
	  new Label("Left Vector"),sliderL,VL, 
      new Label("Right Vector"),sliderR,VR,
	  new Label("Slice Left"),sliderSliceL,VSL,
	  new Label("Slice Right"),sliderSliceR,VSR
	  )
  	
    // Set Listerners and reactions
      listenTo(sliderL,sliderR,sliderSliceL,sliderSliceR,sliderS,Rd)
	  //listenTo(sliderR)
      reactions += {
        case ValueChanged(`sliderL`) => 
          {VL.text = sliderL.value.toString;vdesc.L=sliderL.value;scroll.repaint()}
        case ValueChanged(`sliderR`) => 
          {VR.text = sliderR.value.toString;vdesc.R=sliderR.value;scroll.repaint()}
        case ValueChanged(`sliderSliceL`) => 
          {VSL.text = sliderSliceL.value.toString;vdesc.SL=sliderSliceL.value;scroll.repaint()}
        case ValueChanged(`sliderSliceR`) => 
          {VSR.text = sliderSliceR.value.toString;vdesc.SR=sliderSliceR.value;scroll.repaint()}
        case ValueChanged(`sliderS`) => 
          {vdesc.S=sliderS.value; ui.preferredSize=(7000*vdesc.S/100,1000*vdesc.S/100)
		  scroll.repaint();scroll.revalidate();ui.revalidate()
		  }
		case ButtonClicked(`Rd`) => 
    	  {vdesc.Rand = Rd.selected;scroll.repaint()}
      }

  }

  val scroll=new ScrollPane{
    contents=ui
    preferredSize = (400,400)
	}

  val split= new SplitPane(Orientation.Horizontal,fp,scroll)
  
  
  
  def top = new MainFrame {
    title = "Vector Structure Display"
    contents = split
  }
}

//---------------------------------------------------------------------------


//                       Drawing and Display


//----------------------------------------------------------------------------
import scala.collection.mutable.Map
import java.awt.Font
import java.awt.BasicStroke
import java.awt.RenderingHints
class GVec[A:ClassManifest](v:vec[A]) extends vec[A] {
val bU=16
val deltay=if(Width>8)50*bU else 12*bU
val GTxyMap=Map[GTa,XYRef]()
val ArxyMap=Map[Ara,XYRef]()
class XYRef(var x:Int=0,var y:Int=0,var drawn:Boolean)

def bounds():(Int,Int)=(v.vSize*bU*14/4,6*deltay)

def drawTrie(g:Graphics2D)={
 g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON)
 g.setFont(new Font("monospaced",Font.BOLD,bU*2))
 g.setStroke(new BasicStroke(2.0F))
 if (v.root!=null)drawNode(v.root,0,10,g)
 }


def drawNode(n:AnyRef,level:Int,x:Int,g:Graphics2D):Int={
    var dx=x
    var zx=x
    var dy=deltay*level+12*bU
    n match {
     case in:Ara=>{
       val sz=in.length-1
       for(i<- 1 to sz){
         dx=drawNode(in(i),level+1,dx,g)
         }
       DrawAra(in,(zx+dx)/2,dy,g)
       DrawLines(in,g)
       zx=dx+5*bU  
       }
     case vn:GTa=>{
       dx=DrawGTa(vn,dx,dy,g)
      }
    }
  dx
}

def DrawLines(in:Ara,g:Graphics2D){
  val dxy=ArxyMap(in)
  val mx=dxy.x
  val my=dxy.y
  val sz=in.length-1
 for(i<- 1 to sz){
    in(i) match {
     case lin:Ara=>{
        val sdxy=ArxyMap(lin)
        line(dxy.x+bU+2*bU*(i-1),dxy.y+4*bU,sdxy.x+bU+2*bU*(lin.length-2)/2,sdxy.y,g)
        }
      case lvn:GTa=>{
         val sdxy=GTxyMap(lvn)
        line(dxy.x+bU+2*bU*(i-1),dxy.y+4*bU,sdxy.x+bU+2*bU*(lvn.length-1)/2,sdxy.y,g)
        }
      case null => println("null line")        
      }
    }
  }

def DrawGTa(vn:GTa,x:Int,y:Int,g:Graphics2D):Int={
  val sz=vn.length
  for(i<- 0 until sz){
     val svXform=g.getTransform()
     g.translate(x+(i-1)*2*bU+2*bU,y+6*bU)
     g.rotate(math.Pi/2)
    g.setColor(Color.blue)	 
    g.drawString(vn(i).toString,0,0)
    g.setTransform(svXform)       
    }
   GTxyMap+= (vn -> new XYRef(x,y,true))
   node(x,y,sz,false,g)+5*bU
}

def DrawAra(in:Ara,x:Int,y:Int,g:Graphics2D):Int={
  val sz=in.length-1
   for(i<- 0 until sz){
     val svXform=g.getTransform()
	 g.setColor(Color.red)
     g.translate(x+i*2*bU,y+6*bU)
     g.rotate(math.Pi/2)   
     if(in(0)!=null){
	   val szs=in(0).asInstanceOf[Array[Int]]
	   g.drawString(szs(i).toString,0,0)
	 }
      g.setTransform(svXform)        
     }
   ArxyMap+=(in -> new XYRef(x,y,true))
   node(x,y,sz,in(0)!=null,g)
 }

 def line(x:Int,y:Int,x1:Int,y1:Int,g:Graphics2D){
  g.draw(new geom.Line2D.Double(x,y,x1,y1))
  }

def node(x:Int,y:Int,sz:Int,iscn:Boolean,g:Graphics2D):Int={
  g.setColor(if(iscn)Color.blue else Color.green)
  val svXform=g.getTransform()
  g.translate(x,y)
  for(i<- 0 until sz)g.fill3DRect(i*2*bU,0,2*bU,4*bU,true)
  g.setTransform(svXform)
  x+sz*2*bU
} 
  
}
