package bank

/**
 * identifies uniquely a network element
 * 
 * It works just like an id , by linking every network elemnt with a number so that the users can refer to them
 * The difference is the term random that is created in order to make IDs really unique
 * The idea is that if a network is edited by two different persons and they use the same IDs, it will still be possible to merge them 
 */
case class ID(id: Long, random: Long) {
}