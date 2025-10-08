package meta

import org.springframework.mail.MailException
import javax.mail.MessagingException
import org.springframework.mail.MailSender
import org.springframework.mail.SimpleMailMessage

import grails.util.Environment

import grails.plugins.springsecurity.ui.*

/**
 * Simple service for sending emails.
 *
 * Work is planned in the Grails roadmap to implement first-class email
 * support, so there's no point in making this code any more sophisticated
 * 
 * @auther Haotian Sun
 */
class EmailerService {
        boolean transactional = false
        MailSender mailSender
        SimpleMailMessage mailMessage // a "prototype" email instance
        
        def mailService
        def g = new org.codehaus.groovy.grails.plugins.web.taglib.ApplicationTagLib()

        /**
         * Send a list of emails
         *
         * @param mails a list of maps
         */
        def sendEmails(mails) {
                // Build the mail messages
                def messages = []
                for (mail in mails) {
                        // Create a thread safe "sandbox" of the message
                        SimpleMailMessage message = new SimpleMailMessage(mailMessage)
                        message.to = mail.to
                        message.text = mail.text
                        message.subject = mail.subject
                        messages << message
                }
                // Send them all together
                try {
                        println "about to send ${messages.size()} messages to:\n${messages.to.join('\n')}"
                        mailSender.send(messages as SimpleMailMessage[])
                } catch (MailException ex) {
                        println "Failed to send emails"
                        ex.printStackTrace()
                } catch (MessagingException mex){
                        println "Failed to send emails"
                        mex.printStackTrace()
                }
        }
        
        
        def sendRegistrationApprovalRequest(user, fromEmail) {
                def userShowLink = g.createLink(controller: 'user', action: 'show', id: user.id, absolute: true)
                def body = 'Hello keepers of MetaArch,<br /><br />'
                
                body += 'A newly registered account has been verified by the user.  To review '
                body += 'and approve the registration, go to <a href="' + userShowLink + '">' + userShowLink + '</a>.'
                body += '<br /><br />Have a nice day!<br /><br />- The MetaArch Email Service'
                
                mailService.sendMail {
                        to 'metaarch@eol.ucar.edu' // Uncomment this line once this goes to servers!
                        from fromEmail
                        subject '[MetaArch] Registration Approval Request'
                        html body.toString()
                }               
        }
        
        def sendRegistrationApprovedNotification(user) {
                def appLink = g.createLink(uri: "/", absolute: true)
                def name = user.realname
                if (!name) {
                        name = user.username
                }
                
                def body = 'Hello ' + name + ',<br /><br />'
                
                body += 'Your MetaArch account registration request has been approved!  '
                body += 'As a reminder, your username is  <code>' + user.username + '</code>.<br /><br />'
                body += 'To log in and start using MetaArch, please go to '
                body += '<a href="' + appLink + '">' + appLink + '</a>.'
                body += '<br /><br />Have a nice day!<br />- The MetaArch Team at NCAR/EOL'
                
                mailService.sendMail {
                        to user.email
                        from 'metaarch@eol.ucar.edu' // Uncomment this line once this goes to servers!
                        subject '[MetaArch] Account Registration Approved'
                        html body.toString()
                }
        }
        
        def sendDatasetCreationNotification(dataset) {
                // Submit e-mail to metaarch@eol.ucar.edu indicating dataset has been submitted.
                def dsShowLink = g.createLink(controller: 'dataset', action: 'show', id: dataset.id, absolute: true)
                def body = 'Hello keepers of MetaArch,<br /><br />'
                def envStr = ''
                
                if (Environment.current == Environment.DEVELOPMENT) {
                        envStr = '[DEV]-'
                } else if (Environment.current == Environment.TEST) {
                        envStr = '[TEST]-'
                }
                
                body += 'A data set has been created as part of ' + dataset.project.toString() + '.  To view '
                body += 'this data set, go to <a href="' + dsShowLink + '">' + dsShowLink + '</a>.'
                body += '<br /><br />Have a nice day!<br /><br />- The MetaArch Email Service'
                
                mailService.sendMail {
                        to 'metaarch@eol.ucar.edu' // Uncomment this line once this goes to servers!
                        from 'metaarch@eol.ucar.edu'
                        subject envStr + '[MetaArch] New Data Set for ' + dataset.project.toString() + ' (Dataset ID: ' + dataset.id + ')'
                        html body.toString()
                }
        }
        
        def sendArchiveSubmissionNotification(dataset, bodyText) {
                // Submit e-mail to metaarch@eol.ucar.edu indicating dataset has been submitted.
                def dsShowLink = g.createLink(controller: 'dataset', action: 'show', id: dataset.id, absolute: true)
                def dsAllMetadataLink = g.createLink(controller: 'dataset', action: 'allMetadata', id: dataset.id, absolute: true)
                def docInstLink = 'http://dbo.eol.ucar.edu/doc_format_guide.html'
                def postBody = '<br /><br />Have a nice day!<br /><br />- The MetaArch Email Service'
                def preBody = 'Hello keepers of MetaArch,<br /><br />'
                def envStr = ''
                
                if (Environment.current == Environment.DEVELOPMENT) {
                        envStr = '[DEV]-'
                } else if (Environment.current == Environment.TEST) {
                        envStr = '[TEST]-'
                }
                
                //preBody += 'A data set has been submitted to the ' + dataset.project.toString() + ' archive.  To view '
                preBody += 'A data set has been submitted to the ' + dataset.project.toString() + ' archive by ' + dataset.author.realname + '.  To view '
                preBody += 'the submission, go to <a href="' + dsShowLink + '">' + dsShowLink + '</a> (or <a href="' + dsAllMetadataLink + '">' + dsAllMetadataLink + '</a> for the formatted view).'
                preBody += '  The contents of the data set are as follows:<br /><br />'
                
                // Add the pre and post bodies to the body
                def body = preBody + bodyText + postBody
                
                try {
                        mailService.sendMail {
                                to 'metaarch@eol.ucar.edu' // Uncomment this line once this goes to servers!
                                from 'metaarch@eol.ucar.edu'
                                subject envStr + '[MetaArch] Data Set Submission to ' + dataset.project.toString() + ' Archive (Dataset ID: ' + dataset.id + ')'
                                html body.toString()
                        }
                } catch (MailException ex) {
                        log.error("Failed to send emails:\n\t" + ex.printStackTrace())
                        return false
                }               
                
                // And submit e-mail to notify dataset author and point of contact that this dataset has been submitted
                try {
                        def authorBody = ''
                        authorBody += 'Your data set has been submitted to the ' + dataset.project.toString() + ' archive.  To view '
                        authorBody += 'the submission, go to <a href="' + dsShowLink + '">' + dsShowLink + '</a>.'
                        authorBody += '<br /><br />If you have not already done so, please be sure to create documentation for this data set.  '
                        authorBody += 'Instructions for creating documentation can be found at <a href="' + docInstLink + '">' + docInstLink + '</a>.'
                        authorBody += '<br /><br />Have a nice day!<br /><br />- The MetaArch Team at NCAR/EOL'
                        
                        if (dataset.author != dataset.pointOfContact) {
                                authorBody = 'Hello '+dataset.author.realname+' and '+dataset.pointOfContact.realname+',<br /><br />' + authorBody
                                mailService.sendMail {
                                        to dataset.author.email
                                        cc dataset.pointOfContact.email
                                        from 'metaarch@eol.ucar.edu' // Uncomment this line once this goes to servers!
                                        subject envStr + '[MetaArch] Data Set Submission to ' + dataset.project.toString() + ' Archive  (' + dataset.title + ')'
                                        html authorBody.toString()
                                }
                        } else {
                                authorBody = 'Hello '+dataset.author.realname+',<br /><br />' + authorBody
                                mailService.sendMail {
                                        to dataset.author.email
                                        from 'metaarch@eol.ucar.edu' // Uncomment this line once this goes to servers!
                                        subject envStr + '[MetaArch] Data Set Submission to ' + dataset.project.toString() + ' Archive  (' + dataset.title + ')'
                                        html authorBody.toString()
                                }
                        }
                } catch (MailException ex) {
                        log.error("Failed to send emails:\n\t" + ex.printStackTrace())
                        return false
                }
                
                return true                             
        }
}