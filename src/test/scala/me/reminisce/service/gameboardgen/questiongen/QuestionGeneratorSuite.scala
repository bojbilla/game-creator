package me.reminisce.service.gameboardgen.questiongen

import me.reminisce.mongodb.MongoDBEntities._
import me.reminisce.service.gameboardgen.GameboardEntities._
import org.scalatest.FunSuite


class QuestionGeneratorSuite extends FunSuite {

  def createTestPost(message: String, story: String, postType: String,
                     mediaUrl: Option[String] = None, link: Option[String] = None, from: Option[FBFrom] = None): FBPost = {
    val attachments = mediaUrl.map {
      url => List(FBAttachment(media = Some(FBMedia(1, 1, url))))
    }

    val messgaeOpt = if (message != "") Some(message) else None
    val storyopt = if (story != "") Some(story) else None
    FBPost(postId = "NONE", userId = "NONE", tpe = Some(postType),
      attachments = attachments,
      message = messgaeOpt,
      story = storyopt,
      link = link,
      from = from
    )
  }

  test("An image post should become an image subject.") {
    val postMessage = "TestMessage"
    val postStory = "Story"
    val fbImageUrl = "FBImageURL"
    val imageUrl = "ImageURL"
    val imagePost = createTestPost(postMessage, postStory, "photo", Some(imageUrl), Some(fbImageUrl))
    val subject = QuestionGenerator.subjectFromPost(imagePost)
    subject match {
      case ImagePostSubject(text, pImageUrl, facebookImageUrl, tpe, from) =>
        assert(text == postMessage + "\n" + postStory)
        assert(tpe == SubjectType.ImagePost)
        assert(pImageUrl.getOrElse("") == imageUrl)
        assert(facebookImageUrl.getOrElse("") == fbImageUrl)
      case x => fail(s"Wrong subject type extracted : $x.")
    }
  }

  test("A video post should become a video subject.") {
    val postMessage = "TestMessage"
    val postStory = "Story"
    val videoUrl = "VideoURL"
    val thumbnailUrl = "ThumbnailURL"
    val videoPost = createTestPost(postMessage, postStory, "video", Some(thumbnailUrl), Some(videoUrl))
    val subject = QuestionGenerator.subjectFromPost(videoPost)
    subject match {
      case VideoPostSubject(text, thumbUrl, vidUrl, tpe, from) =>
        assert(text == postMessage + "\n" + postStory)
        assert(tpe == SubjectType.VideoPost)
        assert(thumbUrl.getOrElse("") == thumbnailUrl)
        assert(vidUrl.getOrElse("") == videoUrl)
      case x => fail(s"Wrong subject type extracted : $x.")
    }
  }

  test("A link post should become a link subject.") {
    val postMessage = "TestMessage"
    val postStory = "Story"
    val sharedUrl = "SharedURL"
    val thumbnailUrl = "ThumbnailURL"
    val linkPost = createTestPost(postMessage, postStory, "link", Some(thumbnailUrl), Some(sharedUrl))
    val subject = QuestionGenerator.subjectFromPost(linkPost)
    subject match {
      case LinkPostSubject(text, thumbUrl, shrdLink, tpe, from) =>
        assert(text == postMessage + "\n" + postStory)
        assert(tpe == SubjectType.LinkPost)
        assert(thumbUrl.getOrElse("") == thumbnailUrl)
        assert(shrdLink.getOrElse("") == sharedUrl)
      case x => fail(s"Wrong subject type extracted : $x.")
    }
  }

  test("A post that is not a photo, a video or a link should become a text subject.") {
    val postMessage = "TestMessage"
    val postStory = "Story"

    // Those two should not matter
    val sharedUrl = "SharedURL"
    val thumbnailUrl = "ThumbnailURL"
    val textPost = createTestPost(postMessage, postStory, "ND", Some(thumbnailUrl), Some(sharedUrl))
    val subject = QuestionGenerator.subjectFromPost(textPost)
    subject match {
      case TextPostSubject(text, tpe, from) =>
        assert(text == postMessage + "\n" + postStory)
        assert(tpe == SubjectType.TextPost)
      case x => fail(s"Wrong subject type extracted : $x.")
    }
  }

  test("Text post with no type.") {
    val postMessage = "TestMessage"
    val postStory = "Story"

    val textPost = FBPost(postId = "NONE", userId = "NONE", message = Some(postMessage), story = Some(postStory), attachments = None)
    val subject = QuestionGenerator.subjectFromPost(textPost)
    subject match {
      case TextPostSubject(text, tpe, from) =>
        assert(text == postMessage + "\n" + postStory)
        assert(tpe == SubjectType.TextPost)
      case x => fail(s"Wrong subject type extracted : $x.")
    }
  }

  test("From is correctly extracted.") {
    val fromId = "FromId"
    val fromName = "FromName"
    val from = FBFrom(fromId, fromName)
    val post = createTestPost("", "", "", from = Some(from))

    val subject = QuestionGenerator.subjectFromPost(post)

    subject match {
      case TextPostSubject(text, tpe, frm) =>
        frm match {
          case Some(subFrom) =>
            assert(subFrom.userId == fromId)
            assert(subFrom.userName == fromName)
          case None =>
            fail(s"From is not defined.")
        }
      case x => fail(s"Wrong subject extracted : $x.")
    }
  }

  test("Extracting text from post.") {
    val postMessage = "TestMessage"
    val postStory = "Story"

    val testPostStoryMessage = createTestPost(postMessage, postStory, "ND")
    val storyMessage = QuestionGenerator.textFromPost(testPostStoryMessage)
    assert(storyMessage == postMessage + "\n" + postStory)

    val testPostStoryOnly = createTestPost("", postStory, "ND")
    val storyOnly = QuestionGenerator.textFromPost(testPostStoryOnly)
    assert(storyOnly == postStory)

    val testPostMessgaeOnly = createTestPost(postMessage, "", "ND")
    val messageOnly = QuestionGenerator.textFromPost(testPostMessgaeOnly)
    assert(messageOnly == postMessage)

    val testPostNoText = createTestPost("", "", "ND")
    val noText = QuestionGenerator.textFromPost(testPostNoText)
    assert(noText == "")
  }

  test("Extracting src from FBAttachments.") {
    val noneAttachments = None
    assert(QuestionGenerator.srcFromAttachments(noneAttachments).isEmpty)

    val source = "Source"

    val attachment1 = FBAttachment(media = Some(FBMedia(1, 1, source)))
    val attachment2 = FBAttachment(media = None)
    val list1 = List(attachment1, attachment2)
    val list2 = List(attachment2, attachment1)

    assert(QuestionGenerator.srcFromAttachments(Some(list1)).contains(source))
    assert(QuestionGenerator.srcFromAttachments(Some(list2)).isEmpty)
  }

  test("Extracting subject from a page.") {
    val pageNoPhotoNoName = FBPage(id = None, pageId = "PageId", name = None, photos = None, likesNumber = 0)

    val subjectNoPhotoNoName = QuestionGenerator.subjectFromPage(pageNoPhotoNoName)

    assert(subjectNoPhotoNoName.`type` == SubjectType.PageSubject)
    assert(subjectNoPhotoNoName.name == "")
    assert(subjectNoPhotoNoName.pageId == "PageId")
    assert(subjectNoPhotoNoName.photoUrl.isEmpty)

    val fbPhotoNoSource = FBPhoto(id = "", source = None, createdTime = None, tags = None)
    val pageNoPhotoSource = FBPage(None, "PageId", Some("PageName"), photos = Some(fbPhotoNoSource), 0)

    val subjectNoPhotoSource = QuestionGenerator.subjectFromPage(pageNoPhotoSource)

    assert(subjectNoPhotoSource.`type` == SubjectType.PageSubject)
    assert(subjectNoPhotoSource.name == "PageName")
    assert(subjectNoPhotoSource.pageId == "PageId")
    assert(subjectNoPhotoSource.photoUrl.isEmpty)

    val fbPhoto = FBPhoto(id = "", source = Some("PhotoUrl"), createdTime = None, tags = None)
    val fbPage = FBPage(None, "PageId", Some("PageName"), photos = Some(fbPhoto), 0)
    val subject = QuestionGenerator.subjectFromPage(fbPage)

    assert(subject.`type` == SubjectType.PageSubject)
    assert(subject.name == "PageName")
    assert(subject.pageId == "PageId")
    assert(subject.photoUrl.contains("PhotoUrl"))
  }
}
