/**
 * Wire
 * Copyright (C) 2017 Wire Swiss GmbH
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.waz.zclient.controllers

import android.content.Context
import com.waz.ZLog._
import com.waz.model.ConversationData.ConversationType
import com.waz.model._
import com.waz.service.ZMessaging
import com.waz.threading.Threading
import com.waz.utils.events.{EventContext, Signal}
import com.waz.zclient.core.stores.conversation.ConversationChangeRequester
import com.waz.zclient.utils.Callback
import com.waz.zclient.{BaseActivity, Injectable, Injector}

import scala.concurrent.Future

class UserAccountsController(implicit injector: Injector, context: Context, ec: EventContext) extends Injectable {
  import Threading.Implicits.Ui
  private implicit val tag: LogTag = logTagFor[UserAccountsController]

  val zms = inject[Signal[ZMessaging]]

  val accounts = Option(ZMessaging.currentAccounts).fold(Signal.const[Seq[AccountData]](Seq()))( _.loggedInAccounts.map(_.sortBy(acc => (acc.isTeamAccount, acc.id.str))))

  val currentUser = for {
    zms     <- zms
    account <- ZMessaging.currentAccounts.activeAccount
    user    <- account.flatMap(_.userId).fold(Signal.const(Option.empty[UserData]))(accId => zms.usersStorage.signal(accId).map(Some(_)))
  } yield user

  private var _permissions = Set[AccountData.Permission]()

  private var _teamData = Option.empty[TeamData]
  private var _teamId = Option.empty[TeamId]
  private var _teamMembers = Set.empty[UserId]

  val permissions = for {
    zms <- zms
    accountData <- zms.account.accountData
  } yield accountData.selfPermissions

  permissions { p =>
    _permissions = p
  }

  val teamDataSignal = for {
    zms <- zms
    teamData <- zms.teams.selfTeam
  } yield teamData

  teamDataSignal { data => _teamData = data }

  def teamData = _teamData

  val teamMembersSignal = for {
    zms <- zms
    teamMembers <- Signal.future(zms.teams.searchTeamMembers())
  } yield teamMembers.map(_.id)

  teamMembersSignal { members => _teamMembers = members }

  //Things for java
  //TODO hacky mchackerson - needed for the conversation fragment, remove ASAP
  def setIsGroupListener(id: ConvId, callback: Callback[java.lang.Boolean]): Unit =
    for {
      Some(conv) <- zms.map(_.convsStorage).head.flatMap(_.get(id))
      isGroup    <-
        if (conv.team.isEmpty) Future.successful(conv.convType == ConversationType.Group)
        else zms.map(_.membersStorage).head.flatMap(_.getByConv(conv.id)).map(_.map(_.userId).size > 2)
    } callback.callback(isGroup)

  def createAndOpenConversation(users: Array[UserId], requester: ConversationChangeRequester,  activity: BaseActivity): Unit = {
    val createConv = for {
      z <- zms.head
      user <- z.usersStorage.get(z.selfUserId)
      conv <-
        if (users.length == 1 && !isTeamAccount)
          z.convsUi.getOrCreateOneToOneConversation(users.head)
        else
          z.convsUi.createGroupConversation(ConvId(), users, teamId)
    } yield conv

    createConv.map{ convData =>
      val iConv = activity.getStoreFactory.getConversationStore.getConversation(convData.id.str)
      activity.getStoreFactory.getConversationStore.setCurrentConversation(iConv, requester)
    }(Threading.Ui)
  }

  zms.map(_.teamId) { case teamId => _teamId = teamId }

  def teamId = _teamId
  def isTeamAccount = _teamId.isDefined
  def isTeamMember(userId: UserId) = _teamMembers.contains(userId)

  def hasCreateConversationPermission: Boolean = !isTeamAccount || _permissions(AccountData.Permission.CreateConversation)
  def hasRemoveConversationMemberPermission(convId: ConvId): Boolean = !isTeamAccount || _permissions(AccountData.Permission.RemoveConversationMember)
  def hasAddConversationMemberPermission(convId: ConvId): Boolean = !isTeamAccount || _permissions(AccountData.Permission.AddConversationMember)

}
