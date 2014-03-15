import scala.io.Source

object Wordplay {

  def readDictionary(path: String): Array[String] = {
    val file = Source.fromFile(path)
    val words = file.getLines()
    words.toArray
  }

  def buildInvertedIndex(words: Array[String]): Map[Char, Set[Int]] = {

    def processWord(word: List[Char], index: Int, acc: Map[Char, Set[Int]]): Map[Char, Set[Int]] = {
      word match {
        case Nil => acc
        case letter :: tail =>
          val newMap = if (acc.contains(letter)) {
            val newSet = acc(letter) + index
            acc + (letter -> newSet)
          } else {
            acc + (letter -> Set(index))
          }
          processWord(tail, index, newMap)
      }
    }

    def rec(words: List[String], index: Int, acc: Map[Char, Set[Int]]): Map[Char, Set[Int]] = {
        words match {
          case Nil =>
            acc
          case word :: tail =>
            rec(tail, index + 1, processWord(word.toList, index, acc))
        }
    }
    rec(words.toList, 0, Map[Char, Set[Int]]())
  }

  def buildInvertedIndexWithOccurrences(words: Array[String]): Map[Char, Set[(Int, Int)]] = {

    def processWord(word: List[Char], index: Int, acc: Map[Char, Set[(Int, Int)]]): Map[Char, Set[(Int, Int)]] = {
      word match {
        case Nil => acc
        case letter :: tail =>
          val newMap = if (acc.contains(letter)) {
            val tuples = acc(letter)
            val newSet = if (tuples.exists(t => t._1 == index)) {
              val tuple = tuples.filter(_._1 == index).head
              val count = tuple._2
              tuples - tuple + (tuple._1 -> (count + 1))
            } else {
              tuples + (index -> 1)
            }
            acc + (letter -> newSet)

          } else {
            acc + (letter -> Set((index, 1)))
          }
          processWord(tail, index, newMap)
      }
    }

    def rec(words: List[String], index: Int, acc: Map[Char, Set[(Int, Int)]]): Map[Char, Set[(Int, Int)]] = {
      words match {
        case Nil =>
          acc
        case word :: tail =>
          rec(tail, index + 1, processWord(word.toList, index, acc))
      }
    }
    rec(words.toList, 0, Map[Char, Set[(Int, Int)]]())
  }

  def buildInvertedBiIndex(words: Array[String]): Map[String, Set[Int]] = {

    def processWord(word: List[Char], index: Int, acc: Map[String, Set[Int]]): Map[String, Set[Int]] = {
      word match {
        case first :: Nil => acc
        case first :: second :: tail =>
          val bi = (first :: second :: Nil).mkString("")
          val newMap = if (acc.contains(bi)) {
            val newSet = acc(bi) + index
            acc + (bi -> newSet)
          } else {
            acc + (bi -> Set(index))
          }
          processWord(second::tail, index, newMap)
      }
    }

    def rec(words: List[String], index: Int, acc: Map[String, Set[Int]]): Map[String, Set[Int]] = {
      words match {
        case Nil =>
          acc
        case word :: tail =>
          rec(tail, index + 1, processWord(word.toList, index, acc))
      }
    }
    rec(words.toList, 0, Map[String, Set[Int]]())
  }

  def appearsDoubled(letter: Char, invertedBiIndex: Map[String, Set[Int]]): Boolean = {
    val bi = letter.toString * 2
    invertedBiIndex.contains(bi)
  }

  def neverAppearDoubled(letters: List[Char], invertedBiIndex: Map[String, Set[Int]]): List[Char] = {
    def rec(letters: List[Char], acc: List[Char]): List[Char] = {
      letters match {
        case Nil => acc
        case letter :: tail if appearsDoubled(letter, invertedBiIndex) =>
          rec(tail, acc)
        case letter :: tail => rec(tail, letter::acc)
      }
    }
    rec(letters, List[Char]())
  }

  def isPalindrome(word: String): Boolean = {
    val len = word.length
    val mid = len / 2 - 1
    (0 to mid).forall( i => word(i) == word(len - i - 1))
  }

  /**
   * Finds longest palindrome in an array of words
   * First sorts an array based on the length of words
   * Then processes this sorted list and as soon as the palindrome is found,
   * stops whenever the word in a list is one letter shorter than the found palindrome
   * because of possible multiple palindromes of same length.
   * I guess complexity is O(n log(n) m), but should be faster on average,
   * because of the stop condition.
   * @param words - array of words: dictionary
   * @return list of longest palindromes (in case of multiple palindromes of same length)
   */
  def findLongestPalindromes(words: Array[String]): List[String] = {
    def rec(words: List[String], acc: List[String]): List[String] = {
      words match {
        case Nil => acc
        case word::remWords =>
          acc match {
            case head::tail if head.length > words.head.length => acc
            case _ if isPalindrome(word) => rec(remWords, word::acc)
            case _ => rec(remWords, acc)
          }
      }
    }
    rec(words.toList, List[String]())
  }

  def findMostAppearances(invertedIndexWithOccurrs: Map[Char, Set[(Int, Int)]],
                          words: Array[String]): (Char, (String, Int)) = {
    val map = invertedIndexWithOccurrs.map{
      maping => (maping._1, maping._2.tail.foldLeft(maping._2.head) {
        (max, curr) => if (max._2 > curr._2) max else curr
      })
    }
    val maxMapping = map.tail.foldLeft(map.head)((max, curr) => if (max._2._2 < curr._2._2) curr else max)
    (maxMapping._1, (words(maxMapping._2._1), maxMapping._2._2))
  }

  def containsAll(letters: List[Char],
                  invertedIndex: Map[Char, Set[Int]],
                  words: Array[String]): Set[String] = {
    letters.tail.foldLeft(invertedIndex(letters.head)){ (set: Set[Int], letter: Char) =>
      invertedIndex(letter) & set
    }.map(words(_))
  }

  def main(args: Array[String]) {

    //val words = readDictionary("small.txt")
    val words = readDictionary("aWords.txt")
    //val words = readDictionary("SOWPODS_Dictionary.txt")
    val letters = (65 to 90).map(_.toChar).toList
    val vowels = List('A', 'E', 'I', 'O', 'U', 'Y')

    val invertedIndex = buildInvertedIndex(words)
    val invertedBiIndex = buildInvertedBiIndex(words)
    val invertedIndexWithOccurrs = buildInvertedIndexWithOccurrences(words)

    // 1.1. a) what words contain "AA"?
    println("1.1. a) what words contain \"AA\"?")
    val containAA = if (invertedBiIndex.contains("AA")) invertedBiIndex("AA").map(words(_)) else List()
    println(containAA)

    // 1.1. b) what words contain "A"  without "B"?
    val containsA = invertedIndex('A')
    val containsB = invertedIndex('B')
    val containsAnoB = (containsA &~ containsB).map(words(_))

    println("1.1. b) what words contain \"A\"  without \"B\"?")
    println(containsAnoB)

    // 1.2. what letters, if any, never appear doubled?
    val neverDoubled = neverAppearDoubled(letters, invertedBiIndex)
    println("1.2. what letters, if any, never appear doubled?")
    println(neverDoubled)

    // 1.3. what is the longest palindrome?
    // 1. sort the words according to the length of the word
    // 2. process words and as soon as the palindrome is found,
    //    stop when word length goes 1 letter down in words list
    val sortedByLen = words.sortBy(-_.length)
    val longPalindromes = findLongestPalindromes(sortedByLen)
    println("1.3. what is the longest palindrome?")
    println(longPalindromes)

    // 1.4. what words contain all of the vowels and Y, in any order?
    println("1.4. what words contain all of the vowels and Y, in any order?")
    val containsAllVowels = containsAll(vowels, invertedIndex, words)
    println(containsAllVowels)

    // 1.5. what words contain all of the vowels and Y, in alphabetical order?
    println("1.5. what words contain all of the vowels and Y, in alphabetical order?")
    println("not yet")

    // 1.6. what letter makes the most appearances in a single word, and what is that word?
    println("1.6. what letter makes the most appearances in a single word, and what is that word?")
    val mostAppear = findMostAppearances(invertedIndexWithOccurrs, words)
    println(mostAppear)

    // 1.7 what words are the longest anagrams of each other? (single words, please, no phrases)
    println("1.7 what words are the longest anagrams of each other? (single words, please, no phrases)")
    println("not yet")

  }
}
