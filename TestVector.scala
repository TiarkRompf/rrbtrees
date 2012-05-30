/**
 *  Copyright © 2011 Phil Bagwell and Tiark Rompf. This is a prototype implementation, use at your own risk.
 */

package TestVector
import Vector._
import Math._
import scala.util._
// Benchmark version 09.09.2011
object app extends Application {
Tests.resultsFilePath = "d:/RBBResults/example.txt" // set file path for results
val t=1==0 // Debug flag false
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._

val now = new Date
val df = getDateInstance(SHORT)
val tf = getTimeInstance(SHORT)
val dateNow = df format now
val timeNow = tf format now
// Time Stamp Log
Tests.logf("Test Run on " + dateNow + " " + timeNow)
//Speed Tests

Tests.singleVector=true // No concatenation at all just a single vector
Tests.logf("***Single Vectors Only no concatenation***")
Tests.testIndexSpeed

Tests.singleVector=false
Tests.logf("***Two Vectors Only***")
Tests.fSplit = (x:Int)=>true   // Pure standard vectors p=1
Tests.testIndexSpeed

Tests.logf("*** Concat %3 ***")
Tests.fSplit = (x:Int)=>x%3 == 1 // lightly concated p=1/3
Tests.testIndexSpeed

Tests.logf("*** Concat %4 ***")
Tests.fSplit = (x:Int)=>x%4 == 1 // lightly concated p=1/4
Tests.testIndexSpeed

Tests.logf("*** Concat %5 ***")
Tests.fSplit = (x:Int)=>x%5 == 1 // lightly concated p=1/5
Tests.testIndexSpeed

Tests.logf("*** Concat %6 ***")
Tests.fSplit = (x:Int)=>x%6 == 1 // heavily concated p=1/6
Tests.testIndexSpeed

Tests.logf("*** Concat %7 ***")
Tests.fSplit = (x:Int)=>x%7 == 1 // heavily concated p=1/7
Tests.testIndexSpeed

Tests.logf("*** Concat %8 ***")
Tests.fSplit = (x:Int)=>x%8 == 1 // heavier concated p=1/8
Tests.testIndexSpeed

Tests.logf("*** Random Small Vector Concat ***")
Tests.fSplit = (x:Int)=>false  // total randomly concated small vectors of size 1 to 3
Tests.testIndexSpeed


// **************** Concat Cost *************************
Tests.logf("***Standard Vectors***")
Tests.fSplit = (x:Int)=>true   // Pure standard vectors 
Tests.costConc(1000)

Tests.logf("*** Concat %3 ***")
Tests.fSplit = (x:Int)=>x%3 == 1 // lightly concated
Tests.costConc(1000)

Tests.logf("*** Concat %4 ***")
Tests.fSplit = (x:Int)=>x%4 == 1 // lightly concated
Tests.costConc(1000)

Tests.logf("*** Concat %5 ***")
Tests.fSplit = (x:Int)=>x%5 == 1 // lightly concated
Tests.costConc(1000)

Tests.logf("*** Concat %6 ***")
Tests.fSplit = (x:Int)=>x%6 == 1 // heavily concated
Tests.costConc(1000)

Tests.logf("*** Concat %7 ***")
Tests.fSplit = (x:Int)=>x%7 == 1 // heavily concated
Tests.costConc(1000)

Tests.logf("*** Concat %8 ***")
Tests.fSplit = (x:Int)=>x%8 == 1 // heavier concated
Tests.costConc(1000)

Tests.logf("*** Random Small Vector Concat ***")
Tests.fSplit = (x:Int)=>false  // total randomly concated small vectors
Tests.costConc(1000)

Tests.outputResults


/*
 val a=6
  val b=9
  val m=Tests.createvec(0,a)
  val n=Tests.createvec(a,a+b)
  m.printTrie  
  n.printTrie
  val r=m.conca(n)
  r.printTrie
*/ 



//Tests.prepostpend
//	Tests.testcat2(41,65)
//	Tests.multi2(100)
//	Tests.testcat3(1,1,65,1)
//	Tests.multi3(100)
//	Tests.multiRand(1000,10000)
//	Tests.testAllSliceR(10000)
//	Tests.testAllSliceL(10000)
//	Tests.testAllSliceConc(10000,100000)

	
    }
	
object Tests{
var resultsFilePath = "c:/temp/example.txt" // set file path for results

// *********** Results logging to file **********

def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
  val p = new java.io.PrintWriter(f)
  try { op(p) } finally { p.close() }
}
var res = List("Test Results")

import java.io._
def outputResults = {
  printToFile(new File(resultsFilePath))(p => {
  res.reverse.foreach(p.println)
})
}
def logf(s:String) = {
  println(s)
  res= s::res

}


// *********** Index Speed Tests *********** 

def testIndexSpeed{
  Tests.ixTest(65000) // Warm up Jit
  logf("Index Speed Test RadixP")
  
    Rands=12345673
    for(s<-10 to 23){
    val setsize=pow(2,s).toInt
 	ixTest(setsize)
	}
  }
  var singleVector=false
  def ixTest(ss:Int){
    val ni=20
	var tt=0L
    for( t<- 1 to ni) {
	var mv = if(singleVector)createvec(0,ss) else randTestSet(ss)
    val ixst=System.nanoTime()
	var i=0
	var ts=ss
 while(i<ts){
      //val j=abs((i*1234567)%setsize) 
      if (mv.index(i)!=i)println("Test Index",ss,"***Err***",i,mv.index(i))
	  i+=1
	}
    val xtm=System.nanoTime()-ixst
	tt+=xtm/ts
	}
	logf("size,time/index  " + ss.toString+ "," + (tt/ni).toString)
    }  

// *********** Concatenation costs Tests ***********
 	
def costConc(sz:Int){
  Rands=1234567
  val rand=new Random(1234567)
  val cst=new Array[Double](40)
  val cnt=new Array[Int](40)
  for(i<-0 until 40){cst(i)=0;cnt(i)=0} 
   for(i<- 1 to sz){
    val smax=pow(2,10+(rand.nextInt(13))).toInt
    val sza=rand.nextInt(smax)
	val szb=sza
	//val szb=rand.nextInt(smax)
	var ix=(log(sza+szb)/log(2)).toInt+1
    val va=randTestSet(sza)
	val vb=randTestSet(szb)
	val vc=va.conca(vb)
	cst(ix)+=va.cost
	cnt(ix)+=1
    }
	
  for(i<-1 to 24){
	  val ave=if(cnt(i)==0)0 else cst(i)/cnt(i)
	  println(i,pow(2,i).toInt,cnt(i),ave)
	  logf(i.toString + "," + pow(2,i).toInt.toString + "," + cnt(i).toString + "," + ave.toInt.toString) 
	}
  }
  


// *********** Functional Tests to verify correct concatenation *********** 

def prepostpend{
println("PrePost")
 for(s<-1 to 1000){
  println(s)
   val m=createvec(0,9*32-1)
  //val m=randTestSet(9*32-1)
  val a=createvec(0,s)
  //val a=randTestSet(s)
  //a.printTrie
  var pre=a.conca(m)
  var pst=m.conca(a)
  for(i<- 1 to 1000) {pre=a.conca(pre);pst=pst.conca(a)}
  //v.printTrie
  prntTreeStats("pre",s,pre)
  prntTreeStats("post",s,pst)
  }
 }

 def prntTreeStats(ts:String,s:Int,v:vec[Int]){
    val (mxsl,totn,tots,totAs,mxl,totx,maxx)=v.slotCount()
	if((totx/totAs>1)||(maxx>3)){
    println("s,ts,size,nds,ave slot,maxslt,maxlv,avex,maxx",s,ts,v.vSize,totn,tots/totn,mxsl,mxl,totx/totAs,maxx)
	}
	}
  
// Test that all the values stored in the vector correspond to the correct value
// Assumes values in vector start at 0 and have max of sz-1
def testIndex(v:vec[Int],sz:Int){
    for(i<- 0 until sz){
	if(v.index(i)!=i)println("***Err*** Index size,index,val",sz,i,v.index(i))
    }
  if(v.sizeTrie!=sz)println("***Err*** Size incorrect expected size,actual size",sz,v.sizeTrie)
  }
// Test all concats of combinations of two vectors, sizes from 1 to given limit
def multi2(rep:Int){
  println("multi2",rep)
  for(x<-1 to rep;y<-1 to rep){
  println(x,y)
      testcat2(x,y)
  }
}  

// Test concat of two vectors of given sizes
def testcat2(a:Int,b:Int){
  val m=createvec(0,a)
  val n=createvec(a,a+b)
  
  testIndex(m.conca(n),a+b)
  }

  // Test all concats of combinations of three vectors, sizes from 1 to given limit  
def multi3(rep:Int){
  println("multi3",rep)
  for(x<-1 to rep){
    println("MU",x)
    for(y<-1 to rep;z<-1 to rep;r<- 0 to 1)testcat3(x,y,z,r)
    } 
  }  

// Test concat of three vectors of given sizes
def testcat3(a:Int,b:Int,c:Int,R:Int){
  //println(a,b,c,R)
  val m=createvec(0,a)
  val n=createvec(a,a+b)
  val o=createvec(a+b,a+b+c)  
  val tv=if(R==0)(m.conca(n)).conca(o) else m.conca(n.conca(o))
  testIndex(tv,a+b+c)
  }


// Creates a randomly constructed vector
// Then slices into three parts, concatenates the parts
// Then slices into four, concatenates the parts
// Tests that values are still at correct index locations
// repeats for vectors with intial sizes from strt to end
def testAllSliceConc(strt:Int,end:Int){
    println("testAllSliceConc")
	for(i<- strt to end){
    if(i%100==0)println(i)
    Rands=i*12345673
    val v=randTest(0,i,1)
	//v.printTrie
    val szs=v.vSize
    val m1=szs/3
    val m2=m1*2
    val s1=v.sliceR(m1)
    val s2=v.slice(m1,m2)
    val s3=v.sliceL(m2)
    val b=s1.conca(s2.conca(s3))
	//b.printTrie
    val n1=szs/4
    val n2=szs/2
    val n3=szs*3/4
	//println(n1,n2,n3)
    val sb1=b.sliceR(n1)
    val sb2=b.slice(n1,n2)
    val sb3=b.slice(n2,n3)
    val sb4=b.sliceL(n3)
	/*
	println("SB1")
	sb1.printTrie
	println("SB2")
	sb2.printTrie
	println("SB3")
	sb3.printTrie
	println("SB4")
	sb4.printTrie
	*/
    val c=sb1.conca(sb2.conca(sb3.conca(sb4)))
	//c.printTrie
    testIndex(c,szs)
    }	
}

// Create a randomly constructed vector
// Left slice at all possible places
// Test that values are at correct index locations
def testAllSliceL(sz:Int){
  println("testAllSliceL",sz)
  Rands=sz*12345673
   for(k<- 9 to sz){
   if(k%100==0)println(k)
   val s=randTestSet(k)
   val szs=s.vSize
   for(i<-0 to k-1){
     //println("Slice at",i)
     val v=s.sliceL(i)
     for(j<- 0 until szs-i)if(v.index(j)!=j+i)println("Index Error i,j,val",i,j,v.index(j))
     }
   }
 }

// Create a randomly constructed vector
// Left slice at all possible places
// Test that values are at correct index locations
def testAllSliceR(sz:Int){
  println("testAllSliceR",sz)
  Rands=sz*12345673
  for(k<- 1 to sz){
    if(k%100==0)println(k)
    val v=randTest(0,k,1)
    for(i<-1 to k){
      val s=v.sliceR(i)
	  testIndex(s,i)
      }
	}
}

//  *********** Utility functions  ***********

// Create a vector with values in given range  
def createvec(left:Int,right:Int)={
    if((left<0)||(left>right))println("***Test vector Error*** l,r",left,right)
	(left until right).foldLeft(new vec[Int])(_+_)
    }

//  *********** Creation of Randomly Concatenation of vectors *********** 

// Random number generator with good statistical properties   
var Rands=1234567

def Randn(Size:Int):Int={
 Rands+=hash(Rands)+1237
 if(Size<=1)1
 else abs(Rands)%(Size-1)+1
}
val KF = 0x0000FFFF
val K8 = 0x00FF00FF
val K4 = 0x0F0F0F0F
val K2 = 0x33333333
val K1 = 0x55555555

def revbits(x:Int) = {
  var i=x
  i=((i&KF)<<16)+ (i>>>16)  
  i=((i&K8)<<8) + ((i>>>8)&K8)
  i=((i&K4)<<4)+ ((i>>>4)&K4)
  i=((i&K2)<<2)+ ((i>>>2)&K2)
  ((i&K1)<<1)+ ((i>>>1)&K1)
}
def hash(i:Int)=revbits(i*0xa3Cb56b5)*0xa3Cb56b5


// creates randomly constructed vector and tests that values at indexes are correct
// repeats for vectors of sizes from strt to end
def multiRand(strt:Int,end:Int){
  println("multiRand",strt,end)
  for(i<- strt to end){
  if(i%100==0)println(i)
  randTestSet(i)
  }
} 

// creates randomly constructed vector and tests that values at indexes are correct
def randTestSet(setsize:Int)={
  val v=randTest(0,setsize,1)
  testIndex(v,setsize)
  v
  }

  // creates randomly constructed vector by randomly partitioning the given range
// The partitioning is continued recursively.
// When the partion limit is reach a vector is created and concatenated with its partner.
// This resulting vector is returned and concatenation is done at the next level and so on 
var fSplit = (x:Int)=>true // degree of random splitting as function of a random number.

def randTest(left:Int,right:Int,level:Int):vec[Int]={
  val p=Randn(right-left)
  val sizer=right-left
    if((fSplit(Rands))||(right-left<4)||(p<=1)||(p>=right-left)){
    createvec(left,right)
    }
  else {
    var a=randTest(left,left+p,level+1)
    var b=randTest(left+p,right,level+1)
    a.conca(b)
    }
  }
	
}


