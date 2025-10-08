#! /usr/bin/perl -w

package ViewCGI;
use lib ".";
use lib "lib";
use IvenCGI;
our @ISA = ("IvenCGI");

sub buildContent {}

sub buildPageBody {
    my ($self) = @_;

    $self->printProjectMenu();
    $self->buildContent();
}

sub printProjectMenu {
    my ($self) = @_;
    my $project = $self->getProject();

    printf("<table class=menu>\n");
    printf("   <tr>\n");
    printf("      <td colspan=5>\n");
    printf("         <table class=header>\n");
    printf("            <tr>\n");
    printf("               <td class=menuText>\n");
    printf("                  <div><span class=title>TOI:</span> %s through %s</div>\n",
	   $project->getBeginDate(),$project->getEndDate());
    printf("                  <div><span class=title>Dataset Id Prefix:</span> %s.xxx</div>\n",
	   $project->getPrefix());
    printf("                  <div><span class=title>Charge Number:</span> %s</div>\n",
	   $project->getChargeNumber());
    printf("               </td>\n");
    printf("               <td class=menuTitle>%s: Dataset Processing Inventory</td>\n",
	   $project->getId());
    printf("               <td class=menuText>\n");
    printf("                  <div><span class=title>AOI:</span></div>\n");
    printf("                  <div><span class=data>Min Lat: %7.2f, Max Lat: %7.2f</span></div>\n",
	   $project->getMinLatitude(),$project->getMaxLatitude());
    printf("                  <div><span class=data>Min Lon: %7.2f, Max Lon: %7.2f</span></div>\n",
	   $project->getMinLongitude(),$project->getMaxLongitude());
    printf("               </td>\n");
    printf("            </tr>\n");
    printf("         </table>\n");
    printf("      </td>\n");
    printf("   </tr>\n");
    printf("   <tr class=menuOptions>\n");
    printf("      <td><a href=\"/dpgapps/dln/?project=%s\">DLN for %s</a></td>\n",
	   $self->uriEscape($project->getId()),$project->getId());
    printf("      <td>\n");
    printf("         <select name=products onChange=\"javascript: selectProduct('%s',this);\">\n",$project->getId());
    printf("            <option value=-1>Select a Product</option>\n");
    foreach my $product ($self->getProductList($project)) {
	printf("            <option value=\"%s\">%s</option>\n",$product->getTypedId(),$product->getName());
    }
    printf("         </select>\n");
    printf("      </td>\n");
#    printf("      <td>\n");
#    printf("         <select name=users onChange=\"javascript: selectUser(this);\">\n");
#    printf("            <option value=-1>Select a User</option>\n");
#    foreach my $user ($self->getUserList()) {
#	printf("            <option value=%s>%s</option>\n",$user->getId(),$user->getName());
#    }
#    printf("         </select>\n");
#    printf("      </td>\n");
    printf("      <td><a href=\"project_view?project=%s\">Project Page</a></td>\n",$self->uriEscape($project->getId()));
    printf("      <td><a href=\"project_list\">Home</a></td>\n");
    printf("   </tr>\n");
    printf("</table>\n");
}

sub printStatusBar {
    my ($self,$project,$product) = @_;
    my ($total,$done,$excluded,$excluded_and_done) = $self->{"DB"}->getProductStats($product);
    my $width = ($total - $excluded) ? 100 * ($done - $excluded_and_done) / ($total - $excluded) : 0;

    printf("         <table class=statusBar>\n");
    printf("            <tr>\n");
    printf("               <td class=background>\n");
    printf("                  <div class=foreground style=\"width: %d%s;\"></div>\n",int($width),"%");
    printf("               </td>\n");
    printf("            </tr>\n");
    printf("         </table>\n");

    return ($excluded,$total,$done-$excluded_and_done);
}

1;
