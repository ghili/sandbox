
package datasetcontroleur

trait DbTemplateComponent{  
  val dbTemplate:DbTemplate

  trait DbTemplate {
    def execute(query:String)
    def queryForList[T <: AnyRef](query:String, params:Array[Object]):List[T]
    def queryForList[T <: AnyRef](query:String, params:Array[Object], returnType:Class[_]):List[T]
  }
}

