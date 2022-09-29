
#' find the shortest path between the initial node and other node
#'
#' @param graph data.frame
#' @param init_node A number
#' @return a vector containing these shortest path
#' @export





dijkstra <- function(graph,init_node){
  v_1 <- graph$v1
  v_2 <- graph$v2
  w_1 <- graph$w

  #建立最短路径标记向量；距离出发点长度向量；
  vector_simple <- unique(v_1)
  vector_simple[] <- 1
  vector_mark <- vector_simple
  vector_simple[] <- Inf
  vector_distance <- vector_simple
  #初始化第一个结点距离
  vector_simple <- unique(v_1)
  vector_simple_2 <- sort(vector_simple)
  vector_distance[which(vector_simple_2 == init_node)] <- 0

  #计算所有点的个数，并设置计数向量
  amount <- length(vector_simple)
  count_1 <- 0

  #将起点与其他结点的距离通过循环的方式得出来
  while (count_1 < amount){
    #对未标记的值进行距离大小比较，得出最短距离所对应的结点对应数字，并进行标记
    c4 <- which(vector_mark == 1)
    c5 <- vector_distance[c4]#未标记的在距离向量中的元素组成的向量
    suppressWarnings(min_index <- which(vector_distance == min(c5)))#由此式得出下一个被标记的数字在标记向量中的坐标
    vector_mark[min_index] <- 2
    count_1 <- count_1 + 1#多了一个标记就离成功又进了一步
    value_min <- vector_simple[min_index]
    #计算相邻结点的长度
    c1 <- which(v_1 == value_min)
    c2 <- v_2[c1]
    c3 <- w_1[c1]
    #通过for循环单个来看c2中所对应的结点的位置是否要变化
    for(i in c2){
      index_1 <- which(vector_simple == i)
      if(vector_mark[index_1] == 2){
        next
      }else{
        distance_new <- vector_distance[min_index] +c3[which(c2 == i)]
        if(distance_new < vector_distance[index_1]){
          vector_distance[index_1] <- distance_new
        }
      }
    }

  }
  return(vector_distance)
}
