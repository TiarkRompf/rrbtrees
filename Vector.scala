/**
 *  Copyright © 2011 Phil Bagwell and Tiark Rompf. This is a prototype implementation, use at your own risk.
 */

package Vector
// Benchmark Version 09.09.2011
import Math._   
class vec[T:ClassManifest]{
  var root:AnyRef=null
  val Width=8
  val Invar=1   // sets min standard size for a slot ie w-invar
  val Extras=2  	// sets number of extra slots allowed, ie linear search limit 
  var t=1==0  		// set to 1 for debug printing
  var vSize=0  		// size of vector
  var vHw=0  		// height of vector w, w^2, w^3, ...
  var cost=0  		// used for cost accounting not needed in final version
  type Ara=Array[AnyRef]
  type GTa=Array[T]
    
// &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

//                      Slices

// &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
 def slice(left:Int,right:Int):vec[T]={
  val s=sliceR(right)
  //s.printTrie
  s.sliceL(left)
  }
 
def sliceR(right:Int):vec[T]={
  if(t)println("SliceR")
  if((right<vSize)&&(right>=0)&&(root!=null)){
    val (n,hw)=rSliceDown2(root,right-1,vHw,false)
    val nv = new vec[T]
    nv.vSize=right
    nv.root=n
	nv.vHw=hw
    nv  
    }
    else this
  }
  
  def rSliceDown2(n:AnyRef,right:Int,hw:Int,hasLeft:Boolean):(AnyRef,Int)={
    // Works but can be simplified
      val sw=hw/Width
      var is=right/sw
      n match {
        case in:Ara=>{
          val len=in.length-1
          if(in(0)==null){
              // Aligned vector

            val (rhn:AnyRef,hwr:Int)=rSliceDown2(in(is+1),right-is*sw,hw/Width,(is!=0)||hasLeft)
            if(is==0){
              if(hasLeft){
                // has left above so return add level and return right node/height 
                val rcnodes=new Array[AnyRef](2)
				cost+=2
                rcnodes(1)=rhn
				rcnodes(0)=null
                (rcnodes,hw)
              } 
              else (rhn,hwr) // nothing on left above so just return right node and height
             } 
            else{
              // Make copy of remaining left node
              val cnodes=new Array[AnyRef](is+2)
			  cost+=is+2
              for(i<-0 until is)cnodes(i+1)=in(i+1)
              cnodes(is+1)=rhn
			  cnodes(0)=null			  
              (cnodes,hw)
              }
            }
          else{
		    val szs=in(0).asInstanceOf[Array[Int]]
             var ix=right

			while(szs(is)<=ix)is+=1
			ix=ix-(if(is==0)0 else szs(is-1))
            val nn=in.asInstanceOf[Ara](is+1)

            val (rhn,hwr)=rSliceDown2(nn,ix,hw/Width,(is!=0)||hasLeft)
            if(is==0){

              if(hasLeft){
                val rcnodes=new Array[AnyRef](2)
                val rsizes=new Array[Int](1)
				cost+=3
                rcnodes(1)=rhn
                rsizes(0)=right+1 //++++
				rcnodes(0)=rsizes
                (rcnodes,hw)
                }
              else (rhn,hwr)  // nothing on left so return right node and height 
              }
            else{ 
              val cnodes=new Array[AnyRef](is+2)
              val sizes=new Array[Int](is+1)
			  cost+=2*is+1
              for(i<-0 until is){
                 cnodes(i+1)=in(i+1)
                 sizes(i)=szs(i)
                 }				 
			  cnodes(0)=sizes
			  sizes(is)=right+1
			  cnodes(is+1)=rhn
              (cnodes,hw) 		
            }
          }
        }
        case vn:GTa=>{
          // copy up to is
         var lvals=new Array[T](is+1)
		  cost+=is+1
          for(i<-0 to is)lvals(i)=vn(i)
          (lvals,hw)
      }  
    }
  }
  
 def sliceL(left:Int):vec[T]={
  if(left>=vSize)new vec[T]
  else if((left>0)&&(root!=null)){
    val (n,hw)=lSliceDown2(root,left,vHw,false)
    val nv = new vec[T]
    nv.vSize=vSize-left
    nv.root=n
	nv.vHw=hw
    nv  
    }
    else this
  }
  // hasRight flags there are more slots to the right of this one
  def lSliceDown2(n:AnyRef,left:Int,hw:Int,hasRight:Boolean):(AnyRef,Int)={
      val sw=hw/Width
      var is=left/sw
      n match {
        case in:Ara=>{
          val len=in.length-1
            val (inl,ist,ix)=if(in(0)!=null){
			  // is a sized node so find index position
			  if(t)println("A")
			  val szs=in(0).asInstanceOf[Array[Int]]
              var ix=left
              var it=is
			  while(szs(it)<=ix)it+=1
			  ix=ix-(if(it==0)0 else szs(it-1))
              val nn=in.asInstanceOf[Ara](it+1)
              (nn,it,ix) 
              }
            else(in(is+1),is,left-is*sw) 
			if(t)println("B ist,ix",ist,ix)
           val lastslt=len-1                    
            val (lhn,hwr)=lSliceDown2(inl,ix,hw/Width,(ist!=lastslt)||hasRight)
            if(ist==lastslt){ // no more slots to left 
             if(hasRight){
			 if(t)println("C")
                val rcnodes=new Array[AnyRef](2)
 				cost+=2
                rcnodes(1)=lhn
				rcnodes(0)=null
                (rcnodes,hw)
                }
              else (lhn,hwr)  // nothing on left so return right node and height 
              }
            else{
			// has slots on left so copy them across
			if(t)println("D len,ist",len,ist)
              val cnodes=new Array[AnyRef](len-ist+1)
              for(i<-0 until len-ist-1)cnodes(i+2)=in(ist+2+i)
 				val szs=in(0).asInstanceOf[Array[Int]]
                val rsizes=new Array[Int](len-ist)
 			    cost+=2*(len-ist)+1				
                for(i<-0 until len-ist){
				  val sz=if(in(0)!=null)szs(ist+i) else sw*(ist+1+i)
				  rsizes(i)=sz-left
				  }
				cnodes(0)=rsizes
                cnodes(1)=lhn				
              (cnodes,hw)
          }
        }
        case vn:GTa=>{
          // copy from is to end
          val lenv=vn.length
 		  if(t)println("G lenv,is",lenv,is)
         var lvals=new Array[T](lenv-is)
		  cost+=lenv-is
          for(i<-is until lenv)lvals(i-is)=vn(i)
          (lvals,hw)
      }  
    }
  }
    
// &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

//                      Concatenation

// &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

 
def conca (b:vec[T]):vec[T]={
   cost=0
   if(t)println("conca")
   if(vSize==0)b
   else if(b.vSize==0)this
   else{
     /* not needed for debug only
     val hwl=findhw(root) 
     val hwr=findhw(b.root)
     if((hwl!=vHw)||(hwr!=b.vHw))println("HW Err ************",hwl,vHw,hwr,b.vHw)
	 */
	 
	 if(t)println("CST hwl,hwr",vHw,vSize,b.vHw,b.vSize)

	 val tnca=concatSubTree(root,vHw,b.root,b.vHw,true)
     // create new vector 
	 val nvec=new vec[T]
	 nvec.root=if((vHw==Width)&&(b.vHw==Width)&&(vSize+b.vSize<=Width))
	   tnca.asInstanceOf[Ara](1)
	   else setSizes(tnca,max(vHw,b.vHw))
     nvec.vSize=vSize+b.vSize
	 nvec.vHw=findhw(nvec.root)  // should be able to avoid this extra tree traversal
	 if(t)println("Conca - E vHw",nvec.vHw) 
	 nvec.cost=cost
     nvec
     }
}
    
// IsTop 	
 def concatSubTree(til:AnyRef,hwl:Int,tir:AnyRef,hwr:Int,isTop:Boolean):Ara={
    if(t)println("CST hwl,hwr",hwl,hwr)
   if(hwl>hwr){
     // left vector higher than right
     if(t)println("CST hwl>hwr")
     val tnla=til.asInstanceOf[Ara]
     val tnca=concatSubTree(tnla(tnla.length-1),hwl/Width,tir,hwr,false)
     if(t)println("CST RB hwl>hwr")
	 rebalance(tnla,tnca,null,hwl,isTop)	 
     }
   else if(hwl<hwr){
     // right vector higher than left
     if(t)println("CST hwl<hwr")
     val tnra=tir.asInstanceOf[Ara]
     val tnca=concatSubTree(til,hwl,tnra(1),hwr/Width,false) 
     if(t)println("CST RB hwl<hwr")
	 rebalance(null,tnca,tnra,hwr,isTop)
     }
   else if(hwl==Width){ 
     // height = w so at bottom
     val gnla=til.asInstanceOf[GTa]
	 val gnra=tir.asInstanceOf[GTa]
     val lenl=gnla.length
	 val lenr=gnra.length
     if(t)println("CST G hwl==Width lenl,lenr",lenl,lenr)
	 if(isTop&&(lenr+lenl<=Width)){
	   // sum of two less than w so copy into one if at top of vectors
	   val gal=new GTa(lenr+lenl)
	   for(i<-0 until lenl)gal(i)=gnla(i)
	   for(i<-0 until lenr)gal(i+lenl)=gnra(i)
	   val na=new Ara(2)
	   na(0)=null;na(1)=gal
       cost+=lenl+lenr+2	   
	   na
	  }
	 else{
     if(t)println("CST G B")
	   // else simply return the two subtrees as they will be balance
       // at the next level up	   
	   val na=new Ara(3);na(0)=null
	   cost+=3
       na(1)=til
       na(2)=tir	   
	   na	 
	   }
     }	 
   else{
    // two heights the same so move down both   
    if(t)println("CST hwl==hwr ")
    val tnla=til.asInstanceOf[Ara]
    val tnra=tir.asInstanceOf[Ara]
    val tnca=concatSubTree(tnla(tnla.length-1),hwl/Width,tnra(1),hwr/Width,false)
	if(t)println("CST hwl==hwr -------------------------")
	rebalance(tnla,tnca,tnra,hwl,isTop)
    }
   }
  

def rebalance(al:Ara,ac:Ara,ar:Ara,hw:Int,IsTop:Boolean):Ara={
  // Put all the slots at this level in one array ++Note:  This can be avoided by indexing the sub arrays as one
  var allx=0
  // remember Ara(0) is Size 

  val lenl=if(al!=null)al.length-2 else 0
  val lenc=if(ac!=null)ac.length-1 else 0
  val lenr=if(ar!=null)ar.length-2 else 0
  val all=new Ara(lenl+lenc+lenr)
  if(t)println("RB A lenl,lenc,lenr",lenl,lenc,lenr)
  if(lenl>0){for(i<-0 until lenl)all(i)=al(i+1);allx+=lenl} 
  for(i<-0 until lenc)all(i+allx)=ac(i+1);allx+=lenc
  if(lenr>0){for(i<-0 until lenr)all(i+allx)=ar(i+2)}
  
  // shuffle slot sizes to fit invariant
  val (szs,slen)=shuffle(all)
  // Now copy across according to model sizes in szs
  val nall=copyAcross(all,szs,slen,hw)
  // split across two nodes if greater than Width
	// This splitting/copying can be avoided by moving this logic into the copyAcross
	// and only creating one or two arrays as needed. 
  val ra=if(slen<=Width){
    if(t)println("E IsTop",IsTop)
	if(!IsTop){
	if(t)println("H")
      val na=new Ara(2)
	  cost+=2
 	  na(0)=null
	  na(1)=setSizes(nall,hw)
	  na
	  }
	else nall  
    }
  else{
    if(t)println("F")
    val nal=new Ara(Width+1)
    val nar=new Ara(slen-Width+1)
    for(i<-0 until Width)nal(i+1)=nall(i+1)
    for(i<-0 until slen-Width)nar(i+1)=nall(i+Width+1)
	
    val na=new Ara(3)
	cost+=3
 	na(0)=null
	na(1)=setSizes(nal,hw)
	na(2)= setSizes(nar,hw)
	na
    }
	cost+=slen
    if(t)println("EX RB")	 	
    ra
  }
 
 
 // returns an array with the desired slot sizes.
 // This version allows an Extra number of slots however many slots.
 // if the slots are less than w then as many as Extra+1 could be small
 // while if the total number of slots at the level are as great as 2w 
 // then still only Extra can be small 
 
  def shuffle(all:Ara):(Array[Int],Int)={
    val alen=all.length
	val szs=new Array[Int](alen)
	cost+=alen
	var tcnt=0
	// find total slots in the two levels.
    for(i<- 0 until alen){
	  val sz=sizeSlot(all(i))
	  szs(i)=sz
	  tcnt+=sz
	  }
	if(t)for(i<- 0 until alen)println("SHFS",i,szs(i))  
    // Calculate the ideal or effective number slots
	// used to limit number of extra slots.
	val effslt=tcnt/Width + 1
	var nalen=alen
	// note - this makes multiple passes, can be done in one.
	// redistribute the smallest slots until only the allowed extras remain
    while(Extras<nalen-effslt){
      var ix=0
      // skip over any already greater than w-Invar	  
	  while(szs(ix)>Width-Invar)ix+=1
	  var el=szs(ix)
	  if(t)println("SHTB el,ix",el,ix)
	  // Found a short one so redistribute over following ones
	  do{
		val msz=if(szs(ix+1)+el>Width)Width else el+szs(ix+1) 
		szs(ix)=msz
		el=el+szs(ix+1)-msz
	    if(t)println("SHTC el,ix,msz",el,ix,msz)
		ix+=1
	    }while(el>0)
	  if(t)println("SHTC el,ix",el,ix)
	  // shuffle up remaining slot sizes
	  for(i<- ix+1 until nalen)szs(i-1)=szs(i)
	  nalen-=1
      }
	  if(t)for(i<- 0 until nalen-1)println("SHFD",i,szs(i))  
      (szs,nalen)
    }
  
  // Takes the slot size model and copies across slots to match it.	
  def copyAcross(all:Ara,szs:Array[Int],slen:Int,hw:Int):Ara={
 
  val nall=new Ara(slen+1)
  var ix=0  	// index into the all input array
  var offset=0	// offset into an individual slot array. 
				// It points to the next sub tree in the array to be copied
  
  for(i<- 0 until slen){
    if(t)println("RB CA ix,offset,szs(i)",ix,offset,szs(i))	 
    val nsize = szs(i)
   if(t)println("CA Beg ix,offset,nsize",ix,offset,nsize)
    val (ae,asIs)=all(ix) match{
      case a:Ara=>if((offset==0)&&(nsize==a.length-1))(a,true) else (a,false)
	  case g:GTa=>if((offset==0)&&(nsize==g.length))(g,true) else (g,false)
      }	
    val ra=if(asIs){ix+=1;ae}
	else{
    var rta:AnyRef=null
	var fillcnt=0
	var offs=offset
	var nix=ix
    var ga:GTa=null
    var aa:Ara=null
	// collect enough slots together to match the size needed 
 while((fillcnt<nsize)&&(nix<all.length)){
    if(t)println("CA fillcnt,nix,offset,nsize",fillcnt,nix,offs,nsize)
    all(nix) match{
	case aaa:Ara=>{
		aa=if(fillcnt==0) new Ara(nsize+1) else aa
	    val lena=aaa.length-1
	    if(t)println("CA Ara lena,fillcnt,offset",lena,fillcnt,offs)
	    if(nsize-fillcnt>=lena-offs){
	      for(i<-0 until lena-offs)aa(i+fillcnt+1)=aaa(i+offs+1)
		  if(t)println("YY")
		  nix+=1
		  fillcnt+=lena-offs
		  offs=0
	      }
	    else{
	      for(i<-0 until nsize-fillcnt)aa(i+fillcnt+1)=aaa(i+offs+1)
		  if(t)println("YZ")
		  offs+=nsize-fillcnt
		  fillcnt=nsize
	      }
	      rta=aa 
	  }
	case gaa:GTa=>{
		ga=if(fillcnt==0) new GTa(nsize) else ga
	    val lena=gaa.length
	    if(t)println("CA G lena,fillcnt,offset",lena,fillcnt,offs)
	    if(nsize-fillcnt>=lena-offs){
	    if(t)println("CA ZA lena,nsize,fillcnt,offs,galen",lena,nsize,fillcnt,offs,ga.length)
	      for(i<-0 until lena-offs)ga(i+fillcnt)=gaa(i+offs)
		  if(t)println("ZY")
		  fillcnt+=lena-offs
		  nix+=1
		  offs=0
	      }
	    else{
	    if(t)println("CA ZB lena,nsize,fillcnt,offs,galen",lena,nsize,fillcnt,offs,ga.length)
		
	      for(i<-0 until nsize-fillcnt)ga(i+fillcnt)=gaa(i+offs)
		  if(t)println("ZZ")
		  offs+=nsize-fillcnt
		  fillcnt=nsize
	      }
	      rta=ga
	  }
	}
	}
	cost+=nsize
	rta=rta match{
	     case a:Ara=>setSizes(a,hw/Width)
	     case g:GTa=>g
	   }
	ix=nix
	offset=offs
	rta
	}        
	nall(i+1)=ra
    }
	nall
  }
// Finds the sizes of all the sub trees
// Note ** sizes of subtrie should be passed up the tree rather than calculated as an extra traversal

def setSizes(a:Ara,hw:Int)={
  var sigma=0
  val lena=a.length-1
  val szs=new Array[Int](lena)
  cost+=lena
  for(i<- 0 until lena){
	sigma+=sizeSubTrie(a(i+1),hw/Width)
	if(t)println("i,lena,sigma",i,lena,sigma)
    szs(i)=sigma
   }
  a(0)=szs 
  a	
  }

 // Find the size of one array  
def sizeSlot(a:AnyRef)={
  if(a==null){println("sizeSlot NULL");0}
  else{
    a match {
	  case aa:Ara=>aa.length-1 // allow for size in aa(0)
	  case at:GTa=>at.length
	  }
    }
  }

// Finds number of extra slots above those normally needed
// to represent size
def findExtras(inl:Ara,slotsz:Int):(Int)={
  if(inl(0)!=null){
    val inli=inl(0).asInstanceOf[Array[Int]]
    val lenl=inli.length
    val bndry=inli(lenl-1)
    lenl-((bndry-1)/slotsz)-1
  }
  else 0
}
/*
 def equalizeHeight(nl:AnyRef,hwl:Int,hwr:Int,sz:Int):AnyRef={
  var thwl=hwl
  var n=nl
  val hasSizes=nl match{
      case in:Ara=>in(0)!=null
      case _ => false
      }
    while(thwl<hwr){
       var cnodes=new Ara(2)
       var sizes=if(hasSizes){
         val ns=new Array[Int](1)
		 cost+=3
         ns(0)=sz
         ns
         }
         else null
           
       cnodes(0)=sizes
	   cnodes(1)=n
       n=cnodes
       thwl*=Width    
       }
    n   
  }
 */ 
 
// &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

//                 Sizes and heights

// &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
 
def sizeTrie:Int=vSize   // Returns size (length) of vector

//def sizeTrie(tn:AnyRef):Int={sizeSubTrie(tn,heightWeight(tn))}

def sizeSubTrie(tn:AnyRef,hw:Int):Int={
   tn match {
    case in:Ara=>{
      if(in(0)==null){
          val len=in.length
          val sltsz=hw/Width
          sltsz*(len-2)+sizeSubTrie(in(len-1),sltsz)
        }
      else {
	    val sn=in(0).asInstanceOf[Array[Int]]
        sn(sn.length-1)
        }
      }
    case vn:GTa=>vn.length
    }  
  }

/*
  // Finds height as a weight ie Width^h
def heightWeight(n:AnyRef):Int={
  var hw=Width
  var ni=n
  while(ni match{
             case in:Ara=>{ni=in(1);true}
             case vn:GTa=>false
             }
       )hw*=Width
  hw     
  }
*/
  
def findhw(n:AnyRef):Int={
   n match{
     case a:Ara=>findhw(a(1))*Width
	 case g:GTa=>Width
     }
   }
// ************************************************

// All methods before this point needed for concat and slice functionality.
// Index is also needed which is at the end of the file.

// ************************************************
   
   // The following methods implement utilities for constructing test vectors
// They are not the for final implementation  

// ************************************************ 

// Find the height of a Tree 

// ************************************************
/*
def XheightCheck(){
  println("hCkc")
  hChk(root,vHw)
  }
  
def XhChk(n:AnyRef,hw:Int){
  n match{
    case a:Ara=>for(i<- 0 until a.length-2)hChk(a(i+1),hw/Width)
	case g:GTa=>if(hw!=Width)println("HW Err")
    }
  }
*/
  
  // returns the top weight for a given size
  def sztohw(sz:Int)={
  var hw=Width
  while(sz>hw)hw*=Width
  hw
}

 
// &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

//                      Add Item

// &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  def +(value:T)={
 	val nvec=new vec[T]
	val hw=sztohw(vSize)
	nvec.vSize=vSize+1
    nvec.root=if (root==null){
        var a=new Array[T](1)
        a(0)=value
		nvec.vHw=Width
        a
        }
     else {
       val (n,replace)=addVal(root,value,vHw)
       if(replace){nvec.vHw=vHw;n}
       else{
         val a=new Array[AnyRef](3)
         a(0)=null
         a(1)=root
         a(2)=n
		 nvec.vHw=vHw*Width
         a 
         }
       }
	   nvec
     }
 
 
 def addVal(n:AnyRef,value:T,hw:Int):(AnyRef,Boolean)={
   n match {
     case na:Array[AnyRef]=>{
       val lenn=na.length
       val (n,replace)=addVal(na(lenn-1),value,hw/Width)
       if(replace){
         val ta=new Array[AnyRef](lenn)
         for(i<- 0 until lenn-1)ta(i)=na(i)
         na(lenn-1)=n
         (na,true)
         }
       else if(lenn<=Width){
         val ta=new Array[AnyRef](lenn+1)
         for(i<- 0 until lenn)ta(i)=na(i)
         ta(lenn)=n
         (ta,true)
         }
       else{
         val ta=new Array[AnyRef](2)
         ta(0)=null
         ta(1)=n
         (ta,false)
         }   
       }
     case nt:Array[T]=>{
       if(nt.length<Width){
         val la=nt.length
         var a=new Array[T](la+1)
         for(i<-0 until la)a(i)=nt(i)
         a(la)=value
         (a,true) 
         }
       else {
         val b=new Array[T](1)
         b(0)=value
         (b,false)
         } 
       }
     }
   }
 // &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

//           Display Trie - For Testing and Debugging

// &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  
 def printTrie{
  if(t)println("PrintTrie")
 // var hw=Width
 //   while(vSize>hw)hw*=Width
 //   println(vSize,hw,vSize)
    printSubTrie(root,0,vHw,vSize)
    }
   
  def printSubTrie(n:AnyRef,lev:Int,hw:Int,rhs:Int){
  //if(t)println("PrintSubTrie")
     if(n==null)println("null")
     else{   
       var sp=lev.toString + "    "
       for(s<- 0 to lev)sp= sp + "        "
    n match {
     case an:Array[AnyRef]=>{
       val ilen=an.length
       if(an(0)==null)sp+="***"
       for(i<- 1 until ilen){
         val bdry=i*hw/Width
         val sz=if(an(0)==null)min(bdry,rhs)
                else {				
                  val sza=an(0)match {case as:Array[Int]=>as}
                  sza(i-1)
                  }
     sp= sp + " s " + sz.toString
         }
       println(sp)
       for(i<- 1 until ilen){
         val bdry=i*hw/Width
         val nrhs=if(bdry<rhs)bdry else rhs-bdry
         printSubTrie(an(i),lev+1,hw/Width,nrhs)
         }  
       }
     case vn:Array[T]=>{
	   val vlen=vn.length
       for(i<- 0 until vlen){
         sp=sp + " v " + vn(i).toString
         } 
       println(sp)
      } 
    }
  }
}
 // &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

// These two method used for tests only not required in final version

// &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


// Testing & Debugging support
def slotCount():(Int,Int,Int,Int,Int,Int,Int)={
  if(root==null)(0,0,0,0,0,0,0)
  else subcnt(root,1,sztohw(vSize))
}  

// Testing & Debugging support 
def subcnt(n:AnyRef,lev:Int,hw:Int):(Int,Int,Int,Int,Int,Int,Int)={
    var maxslot=0
    var totnodes=0
    var totslots=0
	var totAslots=0
	var totxtr=0
	var maxxtr=0
	var maxl=0
      n match {
        case in:Ara=>{
		  val len=in.length-1
		  totxtr=findExtras(in,hw/Width)
		  maxxtr=totxtr
		  totAslots=1
		  for(i<- 1 to len){
		    val (mxsl,totn,tots,totAs,mxl,totx,maxx)=subcnt(in(i),lev+1,hw/Width)
			maxslot=max(maxslot,mxsl)
			maxslot=max(maxslot,len)
			maxl=max(maxl,mxl)
			maxxtr=max(maxxtr,maxx)
			totnodes+=totn
			totslots+=tots
			totxtr+=totx
			totAslots+=totAs
		    }
		  (maxslot,totnodes+1,totslots+len,totAslots,maxl,totxtr,maxxtr)
		  }
		case vn:GTa=>{
		  (vn.length,1,vn.length,0,lev,0,0)
	      }
		}
  }  
// &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

//                      Update to an Item

// &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

def update (index:Int,value:T):vec[T]={
    if((index<0)||(index>=vSize)||(root==null))this
    else {
	  val hw=sztohw(vSize)
      val nvec=new vec[T]
      nvec.root=updateTrie(root,index,value,vHw)
      nvec.vSize=vSize
	  nvec.vHw=vHw
      nvec
    }
}    

def updateTrie(n:AnyRef,ix:Int,value:T,hw:Int):AnyRef={    
     val sw=hw/Width
     var is=ix/sw
      n match {
        case in:Ara=>{
          val subn=if(in(0)==null)updateTrie(in(is),ix-is*sw,value,hw/Width)
            else{
			  val szs=in(0).asInstanceOf[Array[Int]]
              while(szs(is)<=ix)is+=1
              val nix=ix-(if(is==0)0 else szs(is-1))
              updateTrie(in(is),nix,value,hw/Width)
              }
          val len=in.length-1    
          val cnodes=new Array[AnyRef](len+1)
          for(i<- 0 to len)cnodes(i)=in(i)
          cnodes(is+1)=subn
          cnodes    
          }
        case vn:GTa=>{
          val len=vn.length
          val lvals=new Array[T](len)
          for(i<- 0 until len)lvals(i)=vn(i)
          lvals(is)=value
          lvals
        }   
      }
  }

// &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

//                      Index to an Item

// &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  
 
   
  val S5=1<<5;val S10=1<<10;val S15=1<<15;val S20=1<<20;val S25=1<<25;val S30=1<<30
 
 // Index method optimised for regular vectors
 
    def index(index:Int):T={
	if(Width!=32)indexAll(index)  // Only used for test purposes with vectors not based on 32.
	else{                         // Can be removed for vector implementation
	var ix=index
    def sized(ia:AnyRef,sp:Int):AnyRef={
       val szs=ia.asInstanceOf[Ara](0).asInstanceOf[Array[Int]]
	   
       var is=ix>>sp	
       while(szs(is)<=ix)is+=1
         ix=ix-(if(is==0)0 else szs(is-1))
         ia.asInstanceOf[Ara](is+1)
/*
			  // To eveluate bin or linear search use commented code below here

			 var l = 0 
             var r=szs.length-1
             var m=r/2
             while(r-l>1){
               if(szs(m)>ix)r=m
               else l=m
               m=(r+l)/2
             }
             val ps=if(ix<szs(l))l else r
             ix=ix-(if(ps==0)0 else szs(ps-1))			 
             ia.asInstanceOf[Ara](ps+1)
*/			 
       }
 if((ix<0)||(ix>=vSize))throw new IndexOutOfBoundsException(ix.toString)
	else{
	
	      vHw match { 
		  case S5 => root.asInstanceOf[GTa](ix)
		  case S10 =>{ 
		    val n1=if(root.asInstanceOf[Ara](0)==null)root.asInstanceOf[Ara]((ix>>5)+1) else sized(root,5)
	        n1.asInstanceOf[GTa](ix&31)
	      } 
		  case S15 =>{ 
		    val n1=if(root.asInstanceOf[Ara](0)==null)root.asInstanceOf[Ara]((ix>>10)+1) else sized(root,10)
	        val n2=if(n1.asInstanceOf[Ara](0)==null)n1.asInstanceOf[Ara](((ix>>5)&31)+1) else sized(n1,5)
	        n2.asInstanceOf[GTa](ix&31)
	      } 
		  case S20 =>{
		    val n1=if(root.asInstanceOf[Ara](0)==null)root.asInstanceOf[Ara]((ix>>15)+1) else sized(root,15)
	        val n2=if(n1.asInstanceOf[Ara](0)==null)n1.asInstanceOf[Ara](((ix>>10)&31)+1) else sized(n1,10)
	        val n3=if(n2.asInstanceOf[Ara](0)==null)n2.asInstanceOf[Ara](((ix>>5)&31)+1) else sized(n2,5)
	        n3.asInstanceOf[GTa](ix&31)
	      }
		  case S25 =>{
		    val n1=if(root.asInstanceOf[Ara](0)==null)root.asInstanceOf[Ara]((ix>>20)+1) else sized(root,20)
	        val n2=if(n1.asInstanceOf[Ara](0)==null)n1.asInstanceOf[Ara](((ix>>15)&31)+1) else sized(n1,15)
	        val n3=if(n2.asInstanceOf[Ara](0)==null)n2.asInstanceOf[Ara](((ix>>10)&31)+1) else sized(n2,10)
	        val n4=if(n3.asInstanceOf[Ara](0)==null)n3.asInstanceOf[Ara](((ix>>5)&31)+1) else sized(n3,5)
	        n4.asInstanceOf[GTa](ix&31)
	      } 
		  case S30 =>{
		    val n1=if(root.asInstanceOf[Ara](0)==null)root.asInstanceOf[Ara]((ix>>25)+1) else sized(root,25)
	        val n2=if(n1.asInstanceOf[Ara](0)==null)n1.asInstanceOf[Ara](((ix>>20)&31)+1) else sized(n1,20)
	        val n3=if(n2.asInstanceOf[Ara](0)==null)n2.asInstanceOf[Ara](((ix>>15)&31)+1) else sized(n2,15)
	        val n4=if(n3.asInstanceOf[Ara](0)==null)n3.asInstanceOf[Ara](((ix>>10)&31)+1) else sized(n3,10)
	        val n5=if(n4.asInstanceOf[Ara](0)==null)n4.asInstanceOf[Ara](((ix>>5)&31)+1) else sized(n4,5)
	        n5.asInstanceOf[GTa](ix&31)
	      }		  
	      case _ => throw new IllegalArgumentException()
	      }
	   }
	}
  }

// This method Only used to provide indexing for all Widths not just 32. Not needed in the final version
 // Used here for test purposes and to support display with testing for other values of Width.
 def indexAll(index:Int):T={
    if((index<0)||(index>=vSize))throw new IndexOutOfBoundsException(index.toString)
    var n=root
    var hw=vHw
	var is=0
	//println("IX",index,hw)
    var ix=index
    var vnv:GTa=null
    while(n!=null){
      val sw=hw/Width
      is=ix/sw
      n match {
        case in:Ara=>{
          val len=in.length-1
          if(len<=is)n=null
          else{
            if(in(0)==null){n=in(is+1);ix=ix-is*sw}
            else{
			 val szs=in(0).asInstanceOf[Array[Int]]
			  // To eveluate bin or linear search use commented code below here
/*
			 var l=0
             var r=len-1
             var m=r/2
             while(r-l>1){
               if(szs(m)>ix)r=m
               else l=m
               m=(r+l)/2
             }
             val ps=if(ix<szs(l))l else r 
             n=in(ps+1);ix=ix-(if(ps==0)0 else szs(ps-1))
*/
              // Radix search of level
			  
              while(szs(is)<=ix)is+=1
              ix=ix-(if(is==0)0 else szs(is-1))
              n=in(is+1)
			  
               // End Direct index Search
              } 
            }  
          }
        case vn:GTa=>{
          val len=vn.length
          if(len>is)vnv=vn
          n=null 
        }   
      }
    hw/=Width  
    } 
  vnv(is)	
  }



}

	
	/*
	Test code for alternative indexing implementations
	
		// Pure vector no test for RRM-Trees
	      vHw match { 
		  case S5 => root.asInstanceOf[GTa](ix)
		  case S10 =>{ 
		    val n1=root.asInstanceOf[Ara]((ix>>5)+1)
	        n1.asInstanceOf[GTa](ix&31)
	      } 
		  case S15 =>{ 
		    val n1=root.asInstanceOf[Ara]((ix>>10)+1)
	        val n2=n1.asInstanceOf[Ara](((ix>>5)&31)+1)
	        n2.asInstanceOf[GTa](ix&31)
	      } 
		  case S20 =>{
		    val n1=root.asInstanceOf[Ara]((ix>>15)+1)
	        val n2=n1.asInstanceOf[Ara](((ix>>10)&31)+1)
	        val n3=n2.asInstanceOf[Ara](((ix>>5)&31)+1)
	        n3.asInstanceOf[GTa](ix&31)
	      }
		  case S25 =>{
		    val n1=root.asInstanceOf[Ara]((ix>>20)+1)
	        val n2=n1.asInstanceOf[Ara](((ix>>15)&31)+1)
	        val n3=n2.asInstanceOf[Ara](((ix>>10)&31)+1)
	        val n4=n3.asInstanceOf[Ara](((ix>>5)&31)+1)
	        n4.asInstanceOf[GTa](ix&31)
	      } 
		  case S30 =>{
		    val n1=root.asInstanceOf[Ara]((ix>>25)+1)
	        val n2=n1.asInstanceOf[Ara](((ix>>20)&31)+1)
	        val n3=n2.asInstanceOf[Ara](((ix>>15)&31)+1)
	        val n4=n3.asInstanceOf[Ara](((ix>>10)&31)+1)
	        val n5=n4.asInstanceOf[Ara](((ix>>5)&31)+1)
	        n5.asInstanceOf[GTa](ix&31)
	      }		  
	      case _ => throw new IllegalArgumentException()
	      }
	*/	
	/*
	      if (vHw == (1 << 5)) { 
		    root.asInstanceOf[GTa](ix)// level = 0 (could maybe removed)
	      } 
		  else if (vHw==(1 << 10)) { 
		    val n1=if(root.asInstanceOf[Ara](0)==null)root.asInstanceOf[Ara]((ix>>5)+1) else sized(root,5)
	        n1.asInstanceOf[GTa](ix&31)
	      } 
		  else if (vHw==(1 << 15)){ 
		    val n1=if(root.asInstanceOf[Ara](0)==null)root.asInstanceOf[Ara]((ix>>10)+1) else sized(root,10)
	        val n2=if(n1.asInstanceOf[Ara](0)==null)n1.asInstanceOf[Ara](((ix>>5)&31)+1) else sized(n1,5)
	        n2.asInstanceOf[GTa](ix&31)
	      } 
		  else if (vHw==(1 << 20)){
		    val n1=if(root.asInstanceOf[Ara](0)==null)root.asInstanceOf[Ara]((ix>>15)+1) else sized(root,15)
	        val n2=if(n1.asInstanceOf[Ara](0)==null)n1.asInstanceOf[Ara](((ix>>10)&31)+1) else sized(n1,10)
	        val n3=if(n2.asInstanceOf[Ara](0)==null)n2.asInstanceOf[Ara](((ix>>5)&31)+1) else sized(n2,5)
	        n3.asInstanceOf[GTa](ix&31)
	      }

		  else if (vHw==(1 << 25)){
		    val n1=if(root.asInstanceOf[Ara](0)==null)root.asInstanceOf[Ara]((ix>>20)+1) else sized(root,20)
	        val n2=if(n1.asInstanceOf[Ara](0)==null)n1.asInstanceOf[Ara](((ix>>15)&31)+1) else sized(n1,15)
	        val n3=if(n2.asInstanceOf[Ara](0)==null)n2.asInstanceOf[Ara](((ix>>10)&31)+1) else sized(n2,10)
	        val n4=if(n3.asInstanceOf[Ara](0)==null)n3.asInstanceOf[Ara](((ix>>5)&31)+1) else sized(n3,5)
	        n4.asInstanceOf[GTa](ix&31)
	      } 
		  else if (vHw==(1 << 30)){
		    val n1=if(root.asInstanceOf[Ara](0)==null)root.asInstanceOf[Ara]((ix>>25)+1) else sized(root,25)
	        val n2=if(n1.asInstanceOf[Ara](0)==null)n1.asInstanceOf[Ara](((ix>>20)&31)+1) else sized(n1,20)
	        val n3=if(n2.asInstanceOf[Ara](0)==null)n2.asInstanceOf[Ara](((ix>>15)&31)+1) else sized(n2,15)
	        val n4=if(n3.asInstanceOf[Ara](0)==null)n3.asInstanceOf[Ara](((ix>>10)&31)+1) else sized(n3,10)
	        val n5=if(n4.asInstanceOf[Ara](0)==null)n4.asInstanceOf[Ara](((ix>>5)&31)+1) else sized(n4,5)
	        n5.asInstanceOf[GTa](ix&31)
	      }		  
	      else { // level = 6
	        throw new IllegalArgumentException()
	      }
		 

  
// Constants for index  
val Sbw1=BitWidth  
val Sbw2=2*BitWidth
val Sbw3=3*BitWidth
val Sbw4=4*BitWidth
val Sbw5=5*BitWidth
val SW1=1<<Sbw1
val SW2=1<<Sbw2
val SW3=1<<Sbw3
val SW4=1<<Sbw4
val SW5=1<<Sbw5

  def indexv2(index:Int):T={
    if((index<0)||(index>=vSize))throw new IndexOutOfBoundsException(index.toString)
	else{
    hChk(root,vHw)

    var ix=index
	var is=0
	
	def sized(ia:Ara)={
      // Radix search of level			  
      val szs=ia(0).asInstanceOf[Array[Int]] //match {case si:Array[Int]=>si} //artoia(ia(0))
      while(szs(is)<=ix)is+=1
      ix=ix-(if(is==0)0 else szs(is-1))
      ia(is+1)
      }
	
	def sizedB(ia:Ara)={
	  // Binary search
      val szs=ia(0).asInstanceOf[Array[Int]]
	  var l=0
      var r=ia.length-2
      var m=r/2
      while(r-l>1){
        if(szs(m)>ix)r=m
        else l=m
        m=(r+l)/2
        }
      val ps=if(ix<szs(l))l else r 
      ix=ix-(if(ps==0)0 else szs(ps-1));ia(ps+1);
      }			
			
	root match{
	  case in:Ara=>{
	    if(vHw==1<<10){
	        is=ix>>Sbw1;val nx=if(in(0)!=null) sized(in) else {ix=ix-(is<<Sbw1);in(is+1)}
	        nx match{ 
			  case vn:GTa=>vn(ix)
			  case a:Ara=>{println("ERR");a.asInstanceOf[GTa](ix)}
			 }
	      }
	    else if(vHw==1<<15){
		     hChk(in,vHw)
			 
	        is=ix>>Sbw2
			val nx=if(in(0)!=null) sized(in) else {ix=ix-(is<<Sbw2);in(is+1)}
			nx match{
			  case g:GTa=>{
			  for(i<- 0 until in.length) in(i) match{
			    case a:Ara=>println(i,"ARA")
				case p:GTa=>println(i,"GTA")
				case ai:Array[Int]=>println(i,"AIA")
				case null=>println("NULL")
			    }
				val szs=in(0).asInstanceOf[Array[Int]] //match {case si:Array[Int]=>si} //artoia(ia(0))
                for(i<- 0 until szs.length)println(i,szs(i))
			    println("Ara1 Err",ix,is,index)
				9/0
			    }
			   case a:Ara=>println("ARA")	
			  }
	        is=ix>>Sbw1;val n2= if(nx.asInstanceOf[Ara](0)!=null)sized(nx.asInstanceOf[Ara]) else {ix=ix-(is<<Sbw1);nx.asInstanceOf[Ara](is+1)}
			n2 match{ case vn:GTa=>vn(ix);case a:Ara=>{println("ERR");a.asInstanceOf[GTa](ix)}}
	      }
	    else if(vHw==1<<20){
	        is=ix>>Sbw3;val nx=if(in(0)!=null) sized(in) else {ix=ix-(is<<Sbw3);in(is+1)}
            nx match{case g:GTa=>println("Ara2 Err")}
	        is=ix>>Sbw2;val n2=if(nx.asInstanceOf[Ara](0)!=null)sized(nx.asInstanceOf[Ara](0).asInstanceOf[Ara]) else {ix=ix-(is<<Sbw2);nx.asInstanceOf[Ara](is+1)}
            n2 match{case g:GTa=>println("Ara3 Err")}
	        is=ix>>Sbw1;val n3=if(n2.asInstanceOf[Ara](0)!=null)sized(n2.asInstanceOf[Ara](0).asInstanceOf[Ara]) else {ix=ix-(is<<Sbw1);n2.asInstanceOf[Ara](is+1)}
	        n3 match{ case vn:GTa=>vn(ix);case a:Ara=>{println("ERR");a.asInstanceOf[GTa](ix)}}
	      }
	    else {
	        is=ix>>Sbw4;val nx=if(in(0)!=null) sized(in) else {ix=ix-(is<<Sbw4);in(is+1)}
            nx match{case g:GTa=>println("Ara4 Err")}
	        is=ix>>Sbw3;val n2=if(nx.asInstanceOf[Ara](0)!=null)sized(nx.asInstanceOf[Ara](0).asInstanceOf[Ara]) else {ix=ix-(is<<Sbw3);nx.asInstanceOf[Ara](is+1)}
            n2 match{case g:GTa=>println("Ara5 Err")}
	        is=ix>>Sbw2;val n3=if(n2.asInstanceOf[Ara](0)!=null)sized(n2.asInstanceOf[Ara](0).asInstanceOf[Ara]) else {ix=ix-(is<<Sbw2);n2.asInstanceOf[Ara](is+1)}
            n3 match{case g:GTa=>println("Ara6 Err")}
	        is=ix>>Sbw1;val n4=if(n3.asInstanceOf[Ara](0)!=null)sized(n3.asInstanceOf[Ara](0).asInstanceOf[Ara]) else {ix=ix-(is<<Sbw1);n3.asInstanceOf[Ara](is+1)}
	        n4 match{ case vn:GTa=>vn(ix);case a:Ara=>{println("ERR");a.asInstanceOf[GTa](ix)}}
		   }
		   
       }
	  case vn:GTa=>{vn(ix)}	
	  }
*/

/*
	root match{
	  case vn:GTa=>{println("GA");vn(ix)}
	  case in:Ara=>{
	    println("RR",vHw,SW2,SW3)
	    vHw match{
	      case SW2 =>{
			println("SS")
	        is=ix>>Sbw1;val nx=if(in(0)!=null) sized(in) else {ix=ix-(is<<Sbw1);in(is+1)}
	        nx match{ case vn:GTa=>vn(ix)}
	      }
	      case SW3 =>{
	        is=ix>>Sbw2;val nx=if(in(0)!=null) sized(in) else {ix=ix-(is<<Sbw2);in(is+1)}
	        is=ix>>Sbw1;val n2=nx match{case ia:Ara=>if(ia(0)!=null)sized(ia) else {ix=ix-(is<<Sbw1);ia(is+1)}};
	        n2 match{ case vn:GTa=>vn(ix)}
	      }
	      case SW4 =>{
	        is=ix>>Sbw3;val nx=if(in(0)!=null) sized(in) else {ix=ix-(is<<Sbw3);in(is+1)}
	        is=ix>>Sbw2;val n2=nx match{case ia:Ara=>if(ia(0)!=null)sized(ia) else {ix=ix-(is<<Sbw2);ia(is+1)}}
	        is=ix>>Sbw1;val n3=n2 match{case ia:Ara=>if(ia(0)!=null)sized(ia) else {ix=ix-(is<<Sbw1);ia(is+1)}}
	        n3 match{ case vn:GTa=>vn(ix)}
	      }
	      case SW5=>{
	        is=ix>>Sbw4;val nx=if(in(0)!=null) sized(in) else {ix=ix-(is<<Sbw4);in(is+1)}
	        is=ix>>Sbw3;val n2=nx match{case ia:Ara=>if(ia(0)!=null)sized(ia) else {ix=ix-(is<<Sbw3);ia(is+1)}}
	        is=ix>>Sbw2;val n3=n2 match{case ia:Ara=>if(ia(0)!=null)sized(ia) else {ix=ix-(is<<Sbw2);ia(is+1)}};
	        is=ix>>Sbw1;val n4=n3 match{case ia:Ara=>if(ia(0)!=null)sized(ia) else {ix=ix-(is<<Sbw1);ia(is+1)}}
	        n4 match{ case vn:GTa=>vn(ix)}
		   }
	     }	
       }
	 }
*/	 
//   }
// }

  /*
			To test bin or linear search use this code above
            // Binary search of level
			 var l=0
             var r=len-1
             var m=r/2
             while(r-l>1){
               if(szs(m)>ix)r=m
               else l=m
               m=(r+l)/2
             }
             val ps=if(ix<szs(l))l else r 
             n=in(ps+1);ix=ix-(if(ps==0)0 else szs(ps-1))
             */
             /*
             // Force linear search of level
               is=0 
*/

