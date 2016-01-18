package me.reminisce.database

import com.github.nscala_time.time.Imports._
import me.reminisce.fetching.config.GraphResponses._
import org.scalatest.FunSuite


class MongoDatabaseServiceSuite extends FunSuite {

  test("Convert a page to an FBPage") {
    val convertedAllNone = MongoDatabaseService.pageToFBPage(PageTestsData.allNone)
    assert(convertedAllNone.pageId == PageTestsData.pageId)
    assert(convertedAllNone.photos.isEmpty)
    assert(convertedAllNone.name.isEmpty)
    assert(convertedAllNone.likesNumber == 0)

    val fbPage = MongoDatabaseService.pageToFBPage(PageTestsData.page)

    assert(fbPage.pageId == PageTestsData.pageId)
    assert(fbPage.name.contains(PageTestsData.pageName))
    assert(fbPage.likesNumber == PageTestsData.likes)

    val fbPhoto = fbPage.photos
    fbPhoto match {
      case Some(p) =>
        val fbTags = p.tags
        fbTags match {
          case Some(fbTag1 :: fbTag2 :: Nil) =>
            assert(fbTag1.id.contains(PageTestsData.tag1Id))
            assert(fbTag1.name.contains(PageTestsData.tag1Name))
            assert(fbTag1.createdTime.contains(PageTestsData.tag1Date))
            assert(fbTag1.x.contains(PageTestsData.tag1X))
            assert(fbTag1.y.contains(PageTestsData.tag1Y))

            assert(fbTag2.id.contains(PageTestsData.tag2Id))
            assert(fbTag2.name.contains(PageTestsData.tag2Name))
            assert(fbTag2.createdTime.contains(PageTestsData.tag2Date))
            assert(fbTag2.x.contains(PageTestsData.tag2X))
            assert(fbTag2.y.contains(PageTestsData.tag2Y))
          case Some(t) =>
            fail(s"Wrong number of tags extracted : ${t.length}.")
          case None =>
            fail("Tags not extracted.")
        }

        assert(p.id == PageTestsData.photoId)
        assert(p.source.contains(PageTestsData.photoSource))
        assert(p.createdTime.contains(PageTestsData.photoCreatedTime))
      case None =>
        fail("Photo not extracted.")
    }
  }

  test("Convert a page to an FBPageLike") {
    val fbPageLike = MongoDatabaseService.pageToFBPageLike(PageTestsData.page, PageTestsData.userId)
    assert(fbPageLike.pageId == PageTestsData.pageId)
    assert(fbPageLike.userId == PageTestsData.userId)
    assert(fbPageLike.likeTime == PageTestsData.likedTimeDate)
  }

  test("Convert a post to an FBPost") {
    val convertedAllNone = MongoDatabaseService.postToFBPost(PostTestsData.allNone, PostTestsData.userId)
    assert(convertedAllNone.postId == PostTestsData.postId)
    assert(convertedAllNone.userId == PostTestsData.userId)
    assert(convertedAllNone.message.isEmpty)
    assert(convertedAllNone.story.isEmpty)
    assert(convertedAllNone.place.isEmpty)
    assert(convertedAllNone.createdTime.isEmpty)
    assert(convertedAllNone.from.isEmpty)
    assert(convertedAllNone.likes.isEmpty)
    assert(convertedAllNone.likesCount.isEmpty)
    assert(convertedAllNone.tpe.isEmpty)
    assert(convertedAllNone.link.isEmpty)
    assert(convertedAllNone.attachments.isEmpty)
    assert(convertedAllNone.comments.isEmpty)
    assert(convertedAllNone.commentsCount.isEmpty)

    val fbPost = MongoDatabaseService.postToFBPost(PostTestsData.post, PostTestsData.userId)
    assert(fbPost.userId == PostTestsData.userId)
    assert(fbPost.postId == PostTestsData.postId)
    assert(fbPost.message.contains(PostTestsData.postMessage))
    assert(fbPost.story.contains(PostTestsData.postStory))
    assert(fbPost.createdTime.contains(PostTestsData.postCreatedTime))
    assert(fbPost.likesCount.contains(PostTestsData.rootLikes.data.getOrElse(List()).length))
    assert(fbPost.tpe.contains(PostTestsData.postType))
    assert(fbPost.link.contains(PostTestsData.postLink))
    assert(fbPost.commentsCount.contains(PostTestsData.rootComments.data.getOrElse(List()).length))

    fbPost.place match {
      case Some(place) =>
        assert(place.id.contains(PostTestsData.placeId))
        assert(place.name == PostTestsData.placeName)
        assert(place.createdTime.contains(PostTestsData.placeCreated))
        val loc = place.location
        assert(loc.city.contains(PostTestsData.locationCity))
        assert(loc.country.contains(PostTestsData.locationCountry))
        assert(loc.latitude == PostTestsData.locationLatitude)
        assert(loc.longitude == PostTestsData.locationLongitude)
        assert(loc.street.contains(PostTestsData.locationStreet))
        assert(loc.zip.contains(PostTestsData.locationZip))
      case None =>
        fail("Place not extracted.")
    }

    fbPost.from match {
      case Some(frm) =>
        assert(frm.userId == PostTestsData.fromId)
        assert(frm.userName == PostTestsData.fromName)
      case None =>
        fail("From not extracted.")
    }

    fbPost.likes match {
      case Some(like1 :: like2 :: Nil) =>
        assert(like1.userId == PostTestsData.likeId1)
        assert(like1.userName == PostTestsData.likeName1)

        assert(like2.userId == PostTestsData.likeId2)
        assert(like2.userName == PostTestsData.likeName2)
      case Some(likes) =>
        fail(s"Wrong number of likes extracted : ${likes.length}")
      case None =>
        fail("Likes not extracted.")
    }

    fbPost.attachments match {
      case Some(attach1 :: attach2 :: Nil) =>
        assert(attach1.tpe.contains(PostTestsData.attachmentType1))
        assert(attach1.description.contains(PostTestsData.attachmentDescription1))
        attach1.media match {
          case Some(media) =>
            assert(media.height == PostTestsData.attachImgHeight1)
            assert(media.width == PostTestsData.attachImgWidth1)
            assert(media.src == PostTestsData.attachImgSrc1)
          case None =>
            fail("Media1 not extracted.")
        }

        assert(attach2.tpe.contains(PostTestsData.attachmentType2))
        assert(attach2.description.contains(PostTestsData.attachmentDescription2))
        attach2.media match {
          case Some(media) =>
            assert(media.height == PostTestsData.attachImgHeight2)
            assert(media.width == PostTestsData.attachImgWidth2)
            assert(media.src == PostTestsData.attachImgSrc2)
          case None =>
            fail("Media2 not extracted.")
        }
      case Some(attachments) =>
        fail(s"Wrong number of attachments extracted : ${attachments.length}")
      case None =>
        fail("Attachments not extracted.")
    }

    fbPost.comments match {
      case Some(comm1 :: comm2 :: Nil) =>
        assert(comm1.from.userId == PostTestsData.commFromId1)
        assert(comm1.from.userName == PostTestsData.commFromName1)
        assert(comm1.id == PostTestsData.comment1Id)
        assert(comm1.likeCount == PostTestsData.commLikesCount1)
        assert(comm1.message == PostTestsData.commMess1)

        assert(comm2.from.userId == PostTestsData.commFromId2)
        assert(comm2.from.userName == PostTestsData.commFromName2)
        assert(comm2.id == PostTestsData.comment2Id)
        assert(comm2.likeCount == PostTestsData.commLikesCount2)
        assert(comm2.message == PostTestsData.commMess2)
      case Some(comments) =>
        fail(s"Wrong number of comments extracted : ${comments.length}")
      case None =>
        fail("Comments not extracted.")
    }
  }

  test("When counting likes one should not trust the root summary.") {
    val fbPost = MongoDatabaseService.postToFBPost(PostTestsData.post, PostTestsData.userId)
    val expectedLikeCount = PostTestsData.post.likes.flatMap(root => root.data.map(likesList => likesList.length))
    assert(fbPost.likesCount == expectedLikeCount)
  }
  test("When counting comments one should not trust the root summary.") {
    val fbPost = MongoDatabaseService.postToFBPost(PostTestsData.post, PostTestsData.userId)
    val expectedCommentsCount = PostTestsData.post.comments.flatMap(root => root.data.map(commentsList => commentsList.length))
    assert(fbPost.commentsCount == expectedCommentsCount)
  }
}

object PageTestsData {
  val pageId = "PageId"
  val allNone = Page(id = PageTestsData.pageId, name = None, photos = None, likes = None, created_time = "NotRequired")
  val userId = "MyUser"
  val likes = 10
  val pageName = "MyFunPage"
  val photoId = "MyFunPhoto"
  val photoSource = "MyFunSource"
  val photoCreatedTime = "2015-09-08T11:57:11+0000"
  val pageCreatedTime = "2015-05-29T14:51:50+0000"
  val tag1Id = "MyFunTag1"
  val tag2Id = "MyFunTag2"
  val tag1Name = "Tag1"
  val tag2Name = "Tag2"
  val tag1Date = DateTime.now
  val tag2Date = DateTime.now - 1.day
  val tag1X = 0.5
  val tag1Y = 0.5
  val tag2X = 0.7
  val tag2Y = 0.7
  val tag1 = Tag(id = Some(tag1Id), name = Some(tag1Name), created_time = Some(tag1Date), x = Some(tag1X), y = Some(tag1Y))
  val tag2 = Tag(id = Some(tag2Id), name = Some(tag2Name), created_time = Some(tag2Date), x = Some(tag2X), y = Some(tag2Y))
  val tagRoot = Root[List[Tag]](data = Some(List(tag1, tag2)), paging = None, summary = None)
  val photo = Photo(id = photoId, source = Some(photoSource), created_time = Some(photoCreatedTime), tags = Some(tagRoot))
  val photoRoot = Root[Photo](data = Some(photo), paging = None, summary = None)
  val page = Page(id = pageId, name = Some(pageName), photos = Some(photoRoot), likes = Some(likes), created_time = pageCreatedTime)
  val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)
  val likedTimeDate = formatter.parseDateTime(pageCreatedTime)
}

object PostTestsData {
  val userId = "My User"
  val postId = "My Fun Post"
  val postMessage = "MyFunMessage"
  val postStory = "MyFunStory"
  val postType = "photo"
  val postLink = "My Fun Url"
  val postCreatedTime = "2013-07-14T13:06:47+0000"

  val fromId = "My From Id"
  val fromName = "My From Name"
  val from = From(id = fromId, name = fromName)

  val locationCity = "Lausanne"
  val locationCountry = "Switzerland"
  val locationLatitude = 44.22
  val locationLongitude = 99.99
  val locationStreet = "Rue de Lausanne 19"
  val locationZip = "1020"
  val postLocation = Location(
    city = Some(locationCity),
    country = Some(locationCountry),
    latitude = Some(locationLatitude),
    longitude = Some(locationLongitude),
    street = Some(locationStreet),
    zip = Some(locationZip))
  val placeId = "PID"
  val placeName = "EPFL"
  val placeCreated = "2014-04-20T09:52:45+0000"
  val postPlace = Place(id = Some(placeId), name = Some(placeName), location = Some(postLocation), created_time = Some(placeCreated))

  val likeId1 = "Like Id1"
  val likeName1 = "Like name 1"
  val likeId2 = "Like Id2"
  val likeName2 = "Like name 2"
  val postLike1 = Like(likeId1, likeName1)
  val postLike2 = Like(likeId2, likeName2)
  val likesSumTotalCount = 123
  val likesSummary = Summary(total_count = likesSumTotalCount)
  val rootLikes = Root[List[Like]](data = Some(List(postLike1, postLike2)), paging = None, summary = Some(likesSummary))

  val attachmentDescription1 = "My fun description 1"
  val attachImgHeight1 = 11
  val attachImgWidth1 = 5
  val attachImgSrc1 = "MY fun source"
  val attachmentImage1 = AttachmentImage(height = attachImgHeight1, width = attachImgWidth1, src = attachImgSrc1)
  val attachmentMedia1 = Media(image = Some(attachmentImage1))
  val attachmentType1 = "A great photo"
  val postAttachment1 = Attachment(description = Some(attachmentDescription1), media = Some(attachmentMedia1), `type` = Some(attachmentType1))

  val attachmentDescription2 = "My fun description 2"
  val attachImgHeight2 = 11
  val attachImgWidth2 = 5
  val attachImgSrc2 = "My fun source"
  val attachmentImage2 = AttachmentImage(height = attachImgHeight2, width = attachImgWidth2, src = attachImgSrc2)
  val attachmentMedia2 = Media(image = Some(attachmentImage2))
  val attachmentType2 = "An awesome photo"
  val postAttachment2 = Attachment(description = Some(attachmentDescription2), media = Some(attachmentMedia2), `type` = Some(attachmentType2))

  val attachmentsRoot = Root[List[Attachment]](data = Some(List(postAttachment1, postAttachment2)), paging = None, summary = None)

  val comment1Id = "com1id"
  val commFromId1 = "From Id 1"
  val commFromName1 = "From name 1"
  val commFrom1 = From(id = commFromId1, name = commFromName1)
  val commLikesCount1 = 23
  val commMess1 = "Comment message 1"
  val commAttachDesc = "Comment attachment description"
  val commAttachImgHeight = 155
  val commAttachImgWidth = 522
  val commAttachSrc = "My fun comment source"
  val commAttachImg = AttachmentImage(height = commAttachImgHeight, width = commAttachImgWidth, src = commAttachSrc)
  val commAttachMedia = Media(image = Some(commAttachImg))
  val commAttachMediaTpe = "Yet another photo"
  val commAttachment = Attachment(description = Some(commAttachDesc), media = Some(commAttachMedia), `type` = Some(commAttachMediaTpe))
  val postComment1 = Comment(id = comment1Id, from = commFrom1, like_count = commLikesCount1, message = commMess1, attachments = Some(commAttachment))

  val comment2Id = "com2id"
  val commFromId2 = "From id 2"
  val commFromName2 = "From name 2"
  val commFrom2 = From(id = commFromId2, name = commFromName2)
  val commLikesCount2 = 232
  val commMess2 = "Comment message 2"

  val postComment2 = Comment(id = comment2Id, from = commFrom2, like_count = commLikesCount2, message = commMess2, attachments = None)

  val commentsSumCount = 8907
  val commentsSum = Summary(total_count = commentsSumCount)
  val rootComments = Root[List[Comment]](data = Option(List(postComment1, postComment2)), paging = None, summary = Some(commentsSum))

  val post = Post(id = postId,
    from = Some(from),
    message = Some(postMessage),
    story = Some(postStory),
    place = Some(postPlace),
    likes = Some(rootLikes),
    `type` = Some(postType),
    link = Some(postLink),
    created_time = Some(postCreatedTime),
    attachments = Some(attachmentsRoot),
    comments = Some(rootComments)
  )

  val allNone = Post(id = postId,
    from = None,
    message = None,
    story = None,
    place = None,
    likes = None,
    `type` = None,
    link = None,
    created_time = None,
    attachments = None,
    comments = None
  )
}